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
    (let* ((expr0 (thing-at-point 'symbol))
	   (expr (if ask (ghc-read-expression expr0) expr0))
	   (file (buffer-name)))
      (with-temp-buffer
	(call-process ghc-module-command nil t nil "type" file expr)
	(message (buffer-substring (point-min) (1- (point-max))))))))

(defun ghc-show-info (&optional ask)
  (interactive "P")
  (if (not (ghc-which ghc-module-command))
      (message "%s not found" ghc-module-command)
    (let* ((expr0 (thing-at-point 'symbol))
	   (expr (if ask (ghc-read-expression expr0) expr0))
	   (file (buffer-name))
	   (buf (get-buffer-create ghc-error-buffer-name)))
      (with-current-buffer buf
        (erase-buffer)
	(insert
	 (with-temp-buffer
	   (call-process ghc-module-command nil t nil "info" file expr)
	   (buffer-substring (point-min) (1- (point-max))))))
      (display-buffer buf))))

(defun ghc-read-expression (default)
  (let ((prompt (format "Expression (%s): " default)))
    (read-string prompt default nil)))

(provide 'ghc-info)
