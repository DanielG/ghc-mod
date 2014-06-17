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

(provide 'ghc-rewrite)
