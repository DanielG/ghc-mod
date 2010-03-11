(require 'flymake)

(defvar ghc-flymake-allowed-file-name-masks
  '("\\.l?hs$" ghc-flymake-init flymake-simple-cleanup flymake-get-real-file-name))

(defvar ghc-flymake-err-line-patterns
  '("^\\(.*\\.l?hs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)" 1 2 3 4))

(add-to-list 'flymake-allowed-file-name-masks
	     ghc-flymake-allowed-file-name-masks)

(add-to-list 'flymake-err-line-patterns
	     ghc-flymake-err-line-patterns)

(defun ghc-flymake-init ()
  (let ((file (file-name-nondirectory (buffer-file-name))))
    (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace)
    (list ghc-module-command (append (ghc-module-command-args)
				     (list "check" file)))))

;; ghc --make GHCMod.hs -outputdir dist/flymake -o dist/flymake/GHCMod

;; (defun ghc-flymake-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list ghc-module-command (append (ghc-module-command-args)
;; 			                (list "check" local-file)))))

(provide 'ghc-flymake)