;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-info.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Nov 15, 2010

;;; Code:

(require 'ghc-func)

(defun ghc-show-type (&optional ask)
  (interactive "P")
  (if (not (ghc-which ghc-module-command))
      (message "%s not found" ghc-module-command)
    (let ((modname (ghc-find-module-name)))
      (if (not modname)
	  (message "module should be specified")
	(ghc-show-type0 ask modname)))))

(defun ghc-show-type0 (ask modname)
  (let* ((expr0 (thing-at-point 'symbol))
	 (expr (if ask (ghc-read-expression expr0) expr0))
	 (cdir default-directory)
	 (file (buffer-name)))
    (with-temp-buffer
      (cd cdir)
      (apply 'call-process ghc-module-command nil t nil
	     `(,@(ghc-make-ghc-options) "type" ,file ,modname ,expr))
      (message (buffer-substring (point-min) (1- (point-max)))))))

(defun ghc-show-info (&optional ask)
  (interactive "P")
  (if (not (ghc-which ghc-module-command))
      (message "%s not found" ghc-module-command)
    (let ((modname (ghc-find-module-name)))
      (if (not modname)
	  (message "module should be specified")
	(ghc-show-info0 ask modname)))))

(defun ghc-show-info0 (ask modname)
  (let* ((expr0 (thing-at-point 'symbol))
	 (expr (if ask (ghc-read-expression expr0) expr0))
	 (cdir default-directory)
	 (file (buffer-name))
	 (buf (get-buffer-create ghc-error-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert
       (with-temp-buffer
	 (cd cdir)
	 (apply 'call-process ghc-module-command nil t nil
		`(,@(ghc-make-ghc-options) "info" ,file ,modname ,expr))
	 (buffer-substring (point-min) (1- (point-max))))))
    (display-buffer buf)))

(defun ghc-read-expression (default)
  (let ((prompt (format "Expression (%s): " default)))
    (read-string prompt default nil)))

(defun ghc-find-module-name ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^module[ ]+\\([^ ]+\\)" nil t)
	(match-string-no-properties 1))))

(provide 'ghc-info)
