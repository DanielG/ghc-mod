(defvar ghc-module-command "ghc-mod"
"*The command name of \"ghc-mod\"")
(defvar ghc-bootstrapped nil)

(defun ghc-bootstrap ()
  (unless ghc-bootstrapped
    (dolist (path (process-lines ghc-module-command "elispPath"))
      (add-to-list 'load-path path)
      (setq ghc-bootstrapped t)))
  (when (fboundp 'ghc-init)
    (ghc-init)))
