;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-command.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Apr 13, 2010

;;; Code:

(require 'ghc-flymake)

(defun ghc-insert-template ()
  (interactive)
  (cond
   ((bobp)
    (ghc-insert-module-template))
   ((save-excursion
      (beginning-of-line)
      (looking-at "^[^ ]+ *::"))
    (ghc-insert-function-template))
   ((ghc-flymake-have-errs-p)
    (ghc-flymake-insert-from-warning))
   (t
    (message "Nothing to be done"))))

(defun ghc-insert-module-template ()
  ;; xxx mod from filename...
  (let ((mod (file-name-sans-extension (buffer-name))))
    (insert "module " mod " where\n")))

(defun ghc-insert-function-template ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\([^ ]+\\) *::")
      (save-match-data
	(forward-line)
	(if (eobp) (insert "\n")))
      (insert (match-string 1) " = undefined\n"))))

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
  (if (buffer-modified-p)
      (save-buffer)
    (flymake-start-syntax-check)))

(provide 'ghc-command)
