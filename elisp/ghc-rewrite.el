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
	(let ((varinfo (car info))
	      (declinfo (cadr info))
	      (cases (caddr info)))
	  (message cases)
	  )
      )
    )
  )

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

(defun ghc-initial-code-from-signature ()
  (interactive)
  (let ((info (ghc-obtain-initial-code-from-signature)))
    (if (null info)
	(message "Cannot obtain initial code")
	(let* ((ln-current (line-number-at-pos))
	       (sort (car info))
	       (pos (cadr info))
	       (ln-end (ghc-sinfo-get-end-line pos))
	       (ln-diff (+ 1 (- ln-end ln-current)))
	       (fns-to-insert (caddr info)))
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
