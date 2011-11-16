;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-comp.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 25, 2009

;;; Code:

(require 'ghc-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customize Variables
;;;

(defvar ghc-idle-timer-interval 30
 "*Period of idele timer in second. When timeout, the names of
unloaded modules are loaded")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constants
;;;

;; must be sorted
(defconst ghc-reserved-keyword-for-bol '("class" "data" "default" "import" "infix" "infixl" "infixr" "instance" "main" "module" "newtype" "type"))

;; must be sorted
(defconst ghc-reserved-keyword '("case" "deriving" "do" "else" "if" "in" "let" "module" "of" "then" "where"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Local Variables
;;;

(defvar ghc-window-configuration nil)

(mapc 'make-variable-buffer-local
      '(ghc-window-configuration))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initializer
;;;

(defvar ghc-module-names nil)   ;; completion for "import"
(defvar ghc-merged-keyword nil) ;; completion for type/func/...
(defvar ghc-language-extensions nil)

(defconst ghc-keyword-prefix "ghc-keyword-")
(defvar ghc-keyword-Prelude nil)
(defvar ghc-loaded-module nil)

(defun ghc-comp-init ()
  (let* ((syms '(ghc-module-names
		ghc-language-extensions
		ghc-keyword-Prelude))
	 (vals (ghc-boot (length syms))))
    (ghc-set syms vals))
  (ghc-add ghc-module-names "qualified")
  (ghc-add ghc-module-names "hiding")
  (ghc-add ghc-language-extensions "LANGUAGE")
  (ghc-merge-keywords '("Prelude"))
  (run-with-idle-timer ghc-idle-timer-interval 'repeat 'ghc-idle-timer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Executing command
;;;

(defun ghc-boot (n)
  (if (not (ghc-which ghc-module-command))
      (message "%s not found" ghc-module-command)
    (ghc-read-lisp-list
     (lambda ()
       (message "Initializing...")
       (call-process ghc-module-command nil t nil "-l" "boot")
       (message "Initializing...done"))
     n)))

(defun ghc-load-modules (mods)
  (if (not (ghc-which ghc-module-command))
      (message "%s not found" ghc-module-command)
    (ghc-read-lisp-list
     (lambda ()
       (message "Loading names...")
       (apply 'call-process ghc-module-command nil t nil
	      `(,@(ghc-make-ghc-options) "-l" "browse" ,@mods))
       (message "Loading names...done"))
     (length mods))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Completion
;;;

(defvar ghc-completion-buffer-name "*Completions*")

(defun ghc-complete ()
  (interactive)
  (if (ghc-should-scroll)
      (ghc-scroll-completion-buffer)
    (ghc-try-complete)))

(defun ghc-should-scroll ()
  (let ((window (ghc-completion-window)))
    (and (eq last-command this-command)
	 window (window-live-p window) (window-buffer window)
	 (buffer-name (window-buffer window)))))

(defun ghc-scroll-completion-buffer ()
  (let ((window (ghc-completion-window)))
    (with-current-buffer (window-buffer window)
      (if (pos-visible-in-window-p (point-max) window)
	  (set-window-start window (point-min))
	(save-selected-window
	  (select-window window)
	  (scroll-up))))))

(defun ghc-completion-window ()
  (get-buffer-window ghc-completion-buffer-name 0))

(defun ghc-try-complete ()
  (let* ((end (point))
	 (symbols (ghc-select-completion-symbol))
	 (beg (ghc-completion-start-point))
	 (pattern (buffer-substring-no-properties beg end))
	 (completion (try-completion pattern symbols)))
    (cond
     ((eq completion t) ;; completed
      ) ;; do nothing
     ((null completion) ;; no completions
      (ding))
     ((not (string= pattern completion)) ;; ???
      (delete-region beg end)
      (insert completion)
      (ghc-reset-window-configuration))
     (t ;; multiple completions
      (let* ((list0 (all-completions pattern symbols))
	     (list (sort list0 'string<)))
	(if (= (length list) 1)
	    (ghc-reset-window-configuration)
	  (ghc-save-window-configuration)
	  (with-output-to-temp-buffer ghc-completion-buffer-name
	    (display-completion-list list pattern))))))))

(defun ghc-save-window-configuration ()
  (unless (get-buffer-window ghc-completion-buffer-name)
    (setq ghc-window-configuration (current-window-configuration))))

(defun ghc-reset-window-configuration ()
  (when ghc-window-configuration
    (set-window-configuration ghc-window-configuration)
    (setq ghc-window-configuration nil)))

(defun ghc-module-completion-p ()
  (or (minibufferp)
      (let ((end (point)))
	(save-excursion
	  (beginning-of-line)
	  (and (looking-at "import ")
	       (not (search-forward "(" end t)))))
      (save-excursion
	(beginning-of-line)
	(looking-at " +module "))))

(defun ghc-select-completion-symbol ()
  (cond
   ((ghc-module-completion-p)
    ghc-module-names)
   ((save-excursion
      (beginning-of-line)
      (looking-at "{-#"))
    ghc-language-extensions)
   ((or (bolp)
	(let ((end (point)))
	  (save-excursion
	    (beginning-of-line)
	    (not (search-forward " " end t)))))
    ghc-reserved-keyword-for-bol)
   (t ghc-merged-keyword)))

(defun ghc-completion-start-point ()
  (save-excursion
    (let ((beg (save-excursion (beginning-of-line) (point)))
	  (regex (if (ghc-module-completion-p) "[ (,`]" "[ (,`.]")))
      (if (re-search-backward regex beg t)
	  (1+ (point))
	beg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Loading keywords
;;;

(add-hook 'find-file-hook 'ghc-import-module)

(defun ghc-import-module ()
  (interactive)
  (when (eq major-mode 'haskell-mode)
    (ghc-load-module-buffer)))

(defun ghc-unloaded-modules (mods)
  (ghc-filter (lambda (mod)
		(and (member mod ghc-module-names)
		     (not (member mod ghc-loaded-module))))
	      mods))

(defun ghc-load-module-all-buffers ()
  (ghc-load-merge-modules (ghc-gather-import-modules-all-buffers)))

(defun ghc-load-module-buffer ()
  (ghc-load-merge-modules (ghc-gather-import-modules-buffer)))

(defun ghc-load-merge-modules (mods)
  (let* ((umods (ghc-unloaded-modules mods))
	 (syms (mapcar 'ghc-module-symbol umods))
	 (names (ghc-load-modules umods)))
    (ghc-set syms names)
    (ghc-merge-keywords umods)))

(defun ghc-merge-keywords (mods)
  (setq ghc-loaded-module (append mods ghc-loaded-module))
  (let* ((modkeys (mapcar 'ghc-module-keyword ghc-loaded-module))
	 (keywords (cons ghc-reserved-keyword modkeys))
	 (uniq-sorted (sort (ghc-uniq-lol keywords) 'string<)))
    (setq ghc-merged-keyword uniq-sorted)))

(defun ghc-module-symbol (mod)
  (intern (concat ghc-keyword-prefix mod)))

(defun ghc-module-keyword (mod)
  (symbol-value (ghc-module-symbol mod)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-gather-import-modules-all-buffers ()
  (let ((bufs (mapcar 'buffer-name (buffer-list)))
	ret)
    (save-excursion
      (dolist (buf bufs (ghc-uniq-lol ret))
	(when (string-match "\\.hs$" buf)
	  (set-buffer buf)
	  (ghc-add ret (ghc-gather-import-modules-buffer)))))))

(defun ghc-gather-import-modules-buffer ()
  (let (ret)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^import\\( *qualified\\)? +\\([^\n ]+\\)" nil t)
	(ghc-add ret (match-string-no-properties 2))
	(forward-line)))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Background Idle Timer
;;;

(defalias 'ghc-idle-timer 'ghc-load-module-all-buffer)

(defun ghc-load-module-all-buffer () nil)

(provide 'ghc-comp)
