;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-flymake.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 12, 2010

;;; Code:

(require 'flymake)

(defvar ghc-error-buffer-name "*GHC Errors*")

(defvar ghc-flymake-allowed-file-name-masks
  '("\\.l?hs$" ghc-flymake-init flymake-simple-cleanup flymake-get-real-file-name))

(defvar ghc-flymake-err-line-patterns
  '("^\\(.*\\.l?hs\\):\\([0-9]+\\):\\([0-9]+\\):[ ]*\\(.+\\)" 1 2 3 4))

(add-to-list 'flymake-allowed-file-name-masks
	     ghc-flymake-allowed-file-name-masks)

(add-to-list 'flymake-err-line-patterns
	     ghc-flymake-err-line-patterns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-flymake-init ()
  (let ((after-save-hook nil))
    (save-buffer))
  (let ((file (file-name-nondirectory (buffer-file-name))))
    (list ghc-module-command (append (ghc-module-command-args)
				     (list "check" file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-display-errors ()
  (interactive)
  (let* ((data (ghc-flymake-data))
	 (buf (get-buffer-create ghc-error-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (ghc-insert-errors data))
    (display-buffer buf)))

(defun ghc-insert-errors (data)
  (let ((title (nth 0 data))
	(errs (nth 1 data)))
    (insert title "\n")
    (dolist (err errs)
      (insert (ghc-replace-character (car err) ghc-null ghc-newline) "\n"))
    (goto-char (point-min))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-flymake-insert-type ()
  (interactive)
  (let ((data (ghc-flymake-first-data)))
    (if (and data
	     (string-match "Inferred type: \\([^:]+ :: \\)\\(forall [^.]+\\. \\)?\\([^\0]*\\)" data))
      (progn
	(beginning-of-line)
	(insert (match-string 1 data) (match-string 3 data) "\n"))
      (message "No inferred type"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-flymake-data ()
  (let* ((line-no (flymake-current-line-no))
         (info (nth 0 (flymake-find-err-info flymake-err-info line-no))))
    (flymake-make-err-menu-data line-no info)))

(defun ghc-flymake-first-data ()
  (nth 0 (nth 0 (nth 1 (ghc-flymake-data)))))

(provide 'ghc-flymake)