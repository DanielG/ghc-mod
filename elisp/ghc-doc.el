;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 25, 2009

(require 'ghc-func)
(require 'ghc-comp)

;;; Code:

(defun ghc-browse-document ()
  (interactive)
  (let* ((mod0 (ghc-extract-module))
	 (mod (ghc-read-module-name mod0))
	 (pkg (ghc-resolve-package-name mod)))
    (ghc-display-document pkg mod)))

(defun ghc-resolve-package-name (mod)
  (with-temp-buffer
    (call-process "ghc-pkg" nil t nil "find-module" "--simple-output" mod)
    (goto-char (point-min))
    (when (looking-at "^\\([^-]+\\)-")
      (match-string-no-properties 1))))

(defun ghc-resolve-document-path (pkg)
  (with-temp-buffer
    (call-process "ghc-pkg" nil t nil "field" pkg "haddock-html")
    (goto-char (point-min))
    (when (looking-at "^haddock-html: \\([^ \n]+\\)$")
      (match-string-no-properties 1))))

(defun ghc-display-document (pkg mod)
  (when (and pkg mod)
    (let* ((mod- (ghc-replace-character mod ?. ?-))
	   (path (ghc-resolve-document-path pkg))
	   (url (format "file://%s/%s.html" path mod-)))
      (browse-url url))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ghc-input-map nil)

(unless ghc-input-map
  (setq ghc-input-map
	(if (boundp 'minibuffer-local-map)
	    (copy-keymap minibuffer-local-map)
	  (make-sparse-keymap)))
  (define-key ghc-input-map "\t" 'ghc-complete))


(provide 'ghc-doc)