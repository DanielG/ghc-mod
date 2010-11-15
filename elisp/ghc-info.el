;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-info.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Nov 15, 2010

;;; Code:

(require 'ghc-func)

(defun ghc-show-type ()
  (interactive)
  (if (not (ghc-which ghc-module-command))
      (message "%s not found" ghc-module-command)
    (let ((expr (thing-at-point 'symbol))
	  (file (buffer-name)))
      (with-temp-buffer
	(call-process ghc-module-command nil t nil "type" file expr)
	(message (buffer-substring (point-min) (1- (point-max))))))))

(provide 'ghc-info)
