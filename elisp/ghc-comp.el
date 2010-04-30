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

(defvar ghc-idle-timer-interval 30)

;; must be sorted
(defvar ghc-reserved-keyword-for-bol '("class" "data" "default" "import" "infix" "infixl" "infixr" "instance" "main" "module" "newtype" "type"))

;; must be sorted
(defvar ghc-reserved-keyword '("case" "deriving" "do" "else" "if" "in" "let" "module" "of" "then" "where"))

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

(defvar ghc-keyword-prefix "ghc-keyword-")
(defvar ghc-keyword-Prelude nil)
(defvar ghc-loaded-module nil)

(defun ghc-comp-init ()
  (setq ghc-module-names (cons "hiding" (cons "qualified" (ghc-load-keyword "list"))))
  (setq ghc-language-extensions (cons "LANGUAGE" (ghc-load-keyword "lang")))
  (setq ghc-keyword-Prelude (ghc-load-keyword "browse" "Prelude"))
  (setq ghc-loaded-module '("Prelude"))
  (ghc-merge-keywords)
  (run-with-idle-timer ghc-idle-timer-interval 'repeat 'ghc-idle-timer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Executing command
;;;

(defun ghc-load-keyword (&rest args)
  (if (not (ghc-which ghc-module-command))
      (message "%s not found" ghc-module-command)
    (ghc-read-lisp
     (lambda ()
       (let ((msg (mapconcat 'identity (cons ghc-module-command args) " ")))
	 (message "Executing \"%s\"..." msg)
	 (apply 'call-process ghc-module-command nil t nil (cons "-l" args))
	 (message "Executing \"%s\"...done" msg))))))

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

(defun ghc-select-completion-symbol ()
  (cond
   ((or (minibufferp)
	(save-excursion
	  (beginning-of-line)
	  (looking-at "import ")))
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
    (let ((beg (save-excursion (beginning-of-line) (point))))
      (if (re-search-backward "[ (,`]" beg t)
	  (1+ (point))
	beg))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Loading keywords
;;;

(add-hook 'find-file-hook 'ghc-load-module-buffer)

(defun ghc-load-module-buffer ()
  (interactive)
  (when (eq major-mode 'haskell-mode)
    (ghc-load-module-this-buffer)))

(defun ghc-load-module-this-buffer ()
  (dolist (mod (ghc-gather-import-modules))
    (ghc-load-module mod))
  (ghc-merge-keywords))

(defun ghc-load-module (mod)
  (when (and (member mod ghc-module-names)
	     (not (member mod ghc-loaded-module)))
    (let ((keywords (ghc-load-keyword "browse" mod)))
      (when (or (consp keywords) (null keywords))
	(set (intern (concat ghc-keyword-prefix mod)) keywords)
	(setq ghc-loaded-module (cons mod ghc-loaded-module))))))

(defun ghc-merge-keywords ()
  (let* ((modkeys (mapcar 'ghc-module-keyword ghc-loaded-module))
	 (keywords (cons ghc-reserved-keyword modkeys))
	 (uniq-sorted (sort (ghc-uniq-lol keywords) 'string<)))
    (setq ghc-merged-keyword uniq-sorted)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Background Idle Timer
;;;

(defalias 'ghc-idle-timer 'ghc-load-module-this-buffer)

(defun ghc-gather-import-modules ()
  (let ((bufs (mapcar 'buffer-name (buffer-list)))
	ret)
    (save-excursion
      (dolist (buf bufs)
	(when (string-match "\\.hs$" buf)
	  (set-buffer buf)
	  (setq ret (cons (ghc-gather-import-modules-buffer) ret)))))
    (ghc-uniq-lol ret)))

(defun ghc-gather-import-modules-buffer ()
  (let (ret)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^import *\\([^\n ]+\\)" nil t)
	(setq ret (cons (match-string-no-properties 1) ret))
	(forward-line)))
    ret))

(defun ghc-module-keyword (mod)
  (symbol-value (intern (concat ghc-keyword-prefix mod))))

(provide 'ghc-comp)
