;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-rewrite.el
;;;

;; Author:  Alejandro Serrano <trupill@gmail.com>
;; Created: Jun 17, 2014

;;; Code:

(require 'ghc-func)
(require 'ghc-process)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Case splitting
;;;

(ghc-defstruct sinfo beg-line beg-column end-line end-column info)

(defun ghc-case-split ()
  (interactive)
  (let ((info (ghc-obtain-case-split)))
    (if (null info)
	(message "Cannot split in cases")
	(let* ((current-line    (line-number-at-pos))
	       (begin-line      (ghc-sinfo-get-beg-line info))
	       (begin-line-diff (+ 1 (- begin-line current-line)))
	       (begin-line-pos  (line-beginning-position begin-line-diff))
	       (begin-pos       (- (+ begin-line-pos (ghc-sinfo-get-beg-column info)) 1))
	       (end-line        (ghc-sinfo-get-end-line info))
	       (end-line-diff   (+ 1 (- end-line current-line)))
	       (end-line-pos    (line-beginning-position end-line-diff))
	       (end-pos         (- (+ end-line-pos (ghc-sinfo-get-end-column info)) 1)) )
	    (delete-region begin-pos end-pos)
	    (insert (ghc-sinfo-get-info info)) ) )))

(defun ghc-obtain-case-split ()
  (let* ((ln (int-to-string (line-number-at-pos)))
	 (cn (int-to-string (1+ (current-column))))
	 (file (buffer-file-name))
	 (cmd (format "split %s %s %s\n" file ln cn)))
    (ghc-sync-process cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initial code from signature
;;;

(ghc-defstruct icsinfo sort pos fns)

(defun ghc-initial-code-from-signature ()
  (interactive)
  (let ((info (ghc-obtain-initial-code-from-signature)))
    (if (null info)
	(message "Cannot obtain initial code")
	(let* ((ln-current (line-number-at-pos))
	       (sort (ghc-icsinfo-get-sort info))
	       (pos (ghc-icsinfo-get-pos info))
	       (ln-end (ghc-sinfo-get-end-line pos))
	       (ln-diff (+ 1 (- ln-end ln-current)))
	       (fns-to-insert (ghc-icsinfo-get-fns info)))
	  (goto-char (line-end-position ln-diff))
	  (dolist (fn-to-insert fns-to-insert)
	    (if (equal sort "function")
		(newline)
	        (newline-and-indent))
	    (insert fn-to-insert))))))

(defun ghc-obtain-initial-code-from-signature ()
  (let* ((ln (int-to-string (line-number-at-pos)))
	 (cn (int-to-string (1+ (current-column))))
	 (file (buffer-file-name))
	 (cmd (format "sig %s %s %s\n" file ln cn)))
    (ghc-sync-process cmd)))

(provide 'ghc-rewrite)
