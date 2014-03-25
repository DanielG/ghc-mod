;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-command.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Apr 13, 2010

;;; Code:

(require 'ghc-process)
(require 'ghc-check)

(defun ghc-insert-template ()
  (interactive)
  (cond
   ((bobp)
    (ghc-insert-module-template))
   ((ghc-check-overlay-at (point))
    (ghc-check-insert-from-warning))
   (t
    (message "Nothing to be done"))))

(defun ghc-insert-module-template ()
  (let* ((fullname (file-name-sans-extension (buffer-file-name)))
	 (rootdir (ghc-get-project-root))
	 (len (length rootdir))
	 (name (substring fullname (1+ len)))
	 (file (file-name-sans-extension (buffer-name)))
	 (case-fold-search nil)
	 (mod (if (string-match "^[A-Z]" name)
		  (ghc-replace-character name ?/ ?.)
		(if (string-match "^[a-z]" file)
		    "Main"
		  file))))
    (insert "module " mod " where\n")))

;; (defun ghc-capitalize (str)
;;   (let ((ret (copy-sequence str)))
;;     (aset ret 0 (upcase (aref ret 0)))
;;     ret))

(defun ghc-sort-lines (beg end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line
		   (lambda ()
		     (re-search-forward "^import\\( *qualified\\)? *" nil t)
		     nil)
		   'end-of-line)))))

(defun ghc-save-buffer ()
  (interactive)
  ;; fixme: better way then saving?
  (set-buffer-modified-p t)
  (call-interactively 'save-buffer)
  (ghc-check-syntax))

(provide 'ghc-command)
