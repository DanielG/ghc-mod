;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-check.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar  9, 2014

;;; Code:

(require 'ghc-func)
(require 'ghc-process)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stolen from flymake.el
(defface ghc-face-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "orangered"))
    (t
     :inherit error))
  "Face used for marking error lines."
  :group 'ghc)

(defface ghc-face-warn
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "gold"))
    (t
     :inherit warning))
  "Face used for marking warning lines."
  :group 'ghc)

(defvar ghc-check-error-fringe (propertize "!" 'display '(left-fringe exclamation-mark)))

(defvar ghc-check-warning-fringe (propertize "?" 'display '(left-fringe question-mark)))

(defvar ghc-display-error nil
  "*An action to display errors/warnings for 'M-n' and 'M-p:

nil            does not display errors/warnings.
'minibuffer    displays errors/warnings in the minibuffer.
'other-buffer  displays errors/warnings in the other buffer.
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-check-syntax ()
  (interactive)
  (ghc-with-process (ghc-check-send)
		    'ghc-check-callback
		    (lambda () (setq mode-line-process " -:-"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ghc-defstruct hilit-info file line msg err)

(defun ghc-check-send ()
  (let ((file (buffer-file-name)))
    (if ghc-check-command
	(let ((opts (ghc-haskell-list-of-string ghc-hlint-options)))
	  (if opts
	      (format "lint %s %s\n" opts file)
	    (format "lint %s\n" file)))
      (format "check %s\n" file))))

(defun ghc-haskell-list-of-string (los)
  (when los
    (concat "["
	    (mapconcat (lambda (x) (concat "\"" x "\"")) los ", ")
	    "]")))

(defun ghc-check-callback (status)
  (cond
   ((eq status 'ok)
    (let* ((errs (ghc-read-lisp-this-buffer))
	   (infos (ghc-to-info errs)))
      (cond
       (infos
	(let ((file ghc-process-original-file)
	      (buf ghc-process-original-buffer))
	  (ghc-check-highlight-original-buffer file buf infos)))
       (t
	(ghc-with-current-buffer ghc-process-original-buffer
	  (remove-overlays (point-min) (point-max) 'ghc-check t))))
      (ghc-with-current-buffer ghc-process-original-buffer
	(let ((len (length infos)))
	  (if (= len 0)
	      (setq mode-line-process "")
	    (let* ((errs (ghc-filter 'ghc-hilit-info-get-err infos))
		   (elen (length errs))
		   (wlen (- len elen)))
	      (setq mode-line-process (format " %d:%d" elen wlen))))))))
   (t
    (ghc-with-current-buffer ghc-process-original-buffer
      (setq mode-line-process " failed")))))

(defun ghc-to-info (errs)
  ;; [^\t] to include \n.
  (let ((regex "^\\([^\n]*\\):\\([0-9]+\\):\\([0-9]+\\): *\\([^\t]+\\)")
	info infos)
    (dolist (err errs (nreverse infos))
      (when (string-match regex err)
	(let* ((file (expand-file-name (match-string 1 err))) ;; for Windows
	       (line (string-to-number (match-string 2 err)))
	       ;; don't take column to make multiple same errors to a single.
	       (msg (match-string 4 err))
	       (wrn (string-match "^Warning" msg))
	       (info (ghc-make-hilit-info
		      :file file
		      :line line
		      :msg  msg
		      :err  (not wrn))))
	  (unless (member info infos)
	    (ghc-add infos info)))))))

(defun ghc-check-highlight-original-buffer (ofile buf infos)
  (ghc-with-current-buffer buf
    (remove-overlays (point-min) (point-max) 'ghc-check t)
    (save-excursion
      (goto-char (point-min))
      (dolist (info infos)
	(let ((line (ghc-hilit-info-get-line info))
	      (msg  (ghc-hilit-info-get-msg  info))
	      (file (ghc-hilit-info-get-file info))
	      (err  (ghc-hilit-info-get-err  info))
	      beg end ovl)
	  ;; FIXME: This is the Shlemiel painter's algorithm.
	  ;; If this is a bottleneck for a large code, let's fix.
	  (goto-char (point-min))
	  (cond
	   ((string= ofile file)
	    (forward-line (1- line))
	    (while (eq (char-after) 32) (forward-char))
	    (setq beg (point))
	    (forward-line)
	    (setq end (1- (point))))
	   (t
	    (setq beg (point))
	    (forward-line)
	    (setq end (point))))
	  (setq ovl (make-overlay beg end))
	  (overlay-put ovl 'ghc-check t)
	  (overlay-put ovl 'ghc-file file)
	  (overlay-put ovl 'ghc-msg msg)
	  (overlay-put ovl 'help-echo msg)
	  (let ((fringe (if err ghc-check-error-fringe ghc-check-warning-fringe))
		(face (if err 'ghc-face-error 'ghc-face-warn)))
	    (overlay-put ovl 'before-string fringe)
	    (overlay-put ovl 'face face)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-overlay-p (ovl)
  (overlay-get ovl 'ghc-check))

(defun ghc-check-overlay-at (p)
  (ghc-filter 'ghc-overlay-p (overlays-at p)))

(ghc-defstruct file-msgs file msgs)

(defun ghc-get-errors-over-warnings ()
  (let ((ovls (ghc-check-overlay-at (point))))
    (when ovls
      (let ((msgs (mapcar (lambda (ovl) (overlay-get ovl 'ghc-msg)) ovls))
	    (file (overlay-get (car ovls) 'ghc-file))
	    errs wrns)
	(dolist (msg msgs)
	  (if (string-match "^Warning" msg)
	      (ghc-add wrns msg)
	    (ghc-add errs msg)))
	(ghc-make-file-msgs :file file :msgs (nconc errs wrns))))))

(defun ghc-display-errors ()
  (interactive)
  (let ((file-msgs (ghc-get-errors-over-warnings)))
    (if (null file-msgs)
	(message "No errors or warnings")
      (let ((file (ghc-file-msgs-get-file file-msgs))
	    (msgs (ghc-file-msgs-get-msgs file-msgs)))
	(ghc-display
	 nil
	 (lambda ()
	   (insert file "\n\n")
	   (mapc (lambda (x) (insert x "\n\n")) msgs)))))))

(defun ghc-display-errors-to-minibuf ()
  (let ((file-msgs (ghc-get-errors-over-warnings)))
    (if (null file-msgs)
	(message "No errors or warnings")
      (let* ((file (ghc-file-msgs-get-file file-msgs))
	     (msgs (ghc-file-msgs-get-msgs file-msgs))
	     (errmsg (mapconcat 'identity msgs "\n"))
	     (buffile buffer-file-name))
        (if (string-equal buffile file)
            (message "%s" errmsg)
          (message "%s\n\n%s" file errmsg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-goto-prev-error ()
  (interactive)
  (let* ((here (point))
         (ovls0 (ghc-check-overlay-at here))
         (end (if ovls0 (overlay-start (car ovls0)) here))
         (ovls1 (overlays-in (point-min) end))
         (ovls2 (ghc-filter (lambda (ovl) (overlay-get ovl 'ghc-check)) ovls1))
         (pnts (mapcar 'overlay-start ovls2)))
    (if pnts (goto-char (apply 'max pnts))))
  (cond
   ((eq ghc-display-error 'minibuffer) (ghc-display-errors-to-minibuf))
   ((eq ghc-display-error 'other-buffer) (ghc-display-errors))))

(defun ghc-goto-next-error ()
  (interactive)
  (let* ((here (point))
         (ovls0 (ghc-check-overlay-at here))
         (beg (if ovls0 (overlay-end (car ovls0)) here))
         (ovls1 (overlays-in beg (point-max)))
         (ovls2 (ghc-filter (lambda (ovl) (overlay-get ovl 'ghc-check)) ovls1))
         (pnts (mapcar 'overlay-start ovls2)))
    (if pnts (goto-char (apply 'min pnts))))
  (cond
   ((eq ghc-display-error 'minibuffer) (ghc-display-errors-to-minibuf))
   ((eq ghc-display-error 'other-buffer) (ghc-display-errors))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-check-insert-from-warning ()
  (interactive)
  (dolist (data (mapcar (lambda (ovl) (overlay-get ovl 'ghc-msg)) (ghc-check-overlay-at (point))))
    (save-excursion
      (cond
       ((string-match "Inferred type: \\|no type signature:" data)
	(beginning-of-line)
	(insert-before-markers (ghc-extract-type data) "\n"))
       ((string-match "lacks an accompanying binding" data)
	(beginning-of-line)
	(when (looking-at "^\\([^ ]+\\) *::")
	  (save-match-data
	    (forward-line)
	    (if (not (bolp)) (insert "\n")))
	  (insert (match-string 1) " = undefined\n")))
       ;; GHC 7.8 uses Unicode for single-quotes.
       ((string-match "Not in scope: type constructor or class .\\([^\n]+\\)." data)
	(let ((sym (match-string 1 data)))
	  (ghc-ins-mod sym)))
       ((string-match "Not in scope: data constructor .\\([^\n]+\\)." data)
	;; if the type of data constructor, it would be nice.
	(let ((sym (match-string 1 data)))
	  (ghc-ins-mod sym)))
       ((string-match "\n[ ]+.\\([^ ]+\\). is a data constructor of .\\([^\n]+\\).\n" data)
	(let* ((old (match-string 1 data))
	       (type-const (match-string 2 data))
	       (new (format "%s(%s)" type-const old)))
	  (ghc-check-replace old new)))
       ((string-match "Not in scope: .\\([^\n]+\\)." data)
	(let ((sym (match-string 1 data)))
	  (if (or (string-match "\\." sym) ;; qualified
		  (y-or-n-p (format "Import module for %s?" sym)))
	      (ghc-ins-mod sym)
	    (unless (re-search-forward "^$" nil t)
	      (goto-char (point-max))
	      (insert "\n"))
	    (insert "\n" (ghc-enclose sym) " = undefined\n"))))
       ((string-match "Pattern match(es) are non-exhaustive" data)
	(let* ((fn (ghc-get-function-name))
	       (arity (ghc-get-function-arity fn)))
	  (ghc-insert-underscore fn arity)))
       ((string-match "Found:\n[ ]+\\([^\t]+\\)\nWhy not:\n[ ]+\\([^\t]+\\)" data)
	(let ((old (match-string 1 data))
	      (new (match-string 2 data)))
	  (ghc-check-replace old new)))
       (t
	(message "Nothing was done"))))))

(defun ghc-check-replace (old new)
  (beginning-of-line)
  (when (search-forward old nil t)
    (let ((end (point)))
      (search-backward old nil t)
      (delete-region (point) end))
    (insert new)))

(defun ghc-extract-type (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (when (re-search-forward "Inferred type: \\|no type signature:\\( \\|\n +\\)?" nil t)
      (delete-region (point-min) (point)))
    (when (re-search-forward " forall [^.]+\\." nil t)
      (replace-match ""))
    (while (re-search-forward "\n +" nil t)
      (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward "\\[Char\\]" nil t)
      (replace-match "String"))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ghc-get-function-name ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\([^ ]+\\) ")
      (match-string 1))))

(defun ghc-get-function-arity (fn)
  (when fn
    (save-excursion
      (let ((regex (format "^%s *::" (regexp-quote fn))))
	(when (re-search-backward regex nil t)
	  (ghc-get-function-arity0))))))

(defun ghc-get-function-arity0 ()
  (let ((end (save-excursion (end-of-line) (point)))
	(arity 0))
    (while (search-forward "->" end t)
      (setq arity (1+ arity)))
    arity))

(defun ghc-insert-underscore (fn ar)
  (when fn
    (let ((arity (or ar 1)))
      (save-excursion
	(goto-char (point-max))
	(re-search-backward (format "^%s *::" (regexp-quote fn)))
	(forward-line)
	(re-search-forward "^$" nil t)
	(insert fn)
	(dotimes (i arity)
	  (insert " _"))
	(insert  " = error \"" fn "\"\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-jump-file ()
  (interactive)
  (let* ((ovl (car (ghc-check-overlay-at 1)))
	 (file (if ovl (overlay-get ovl 'ghc-file))))
    (if file (find-file file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ghc-hlint-options nil "*Hlint options")

(defvar ghc-check-command nil)

(defun ghc-toggle-check-command ()
  (interactive)
  (setq ghc-check-command (not ghc-check-command))
  (if ghc-check-command
      (message "Syntax check with hlint")
    (message "Syntax check with GHC")))

(provide 'ghc-check)
