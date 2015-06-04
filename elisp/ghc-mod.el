(defconst ghc-version "0")
(defvar ghc-module-command "ghc-mod"
"*The command name of \"ghc-mod\"")
(defvar ghc-bootstrapped nil)

;;;###autoload
(defun ghc-mod-init ()
  (unless ghc-bootstrapped
    (dolist (path (process-lines ghc-module-command "elispPath"))
      (add-to-list 'load-path path)
      (setq ghc-bootstrapped t)))
  (when (fboundp 'ghc-init)
    (ghc-init)))
