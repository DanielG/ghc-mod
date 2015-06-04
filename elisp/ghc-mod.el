(defconst ghc-mod-version "0")
(defvar ghc-module-command "ghc-mod"
"*The command name of \"ghc-mod\"")
(defvar ghc-bootstrapped nil)

;;;###autoload
(defun ghc-mod-bootstrap ()
  (unless ghc-bootstrapped
    (dolist (path (process-lines ghc-module-command "elispPath"))
      (add-to-list 'load-path path)
      (require 'ghc-mod-internal)
      (setq ghc-bootstrapped t))))

;;;###autoload
(defun ghc-mod-init ()
  (ghc-mod-bootstrap)
  (when (fboundp 'ghc-mod-internal-init)
    (ghc-mod-internal-init)))

;;;###autoload
(defun ghc-mod-debug ()
  (condition-case err
      (ghc-mod-bootstrap)
    ('error
     (interactive)
     (switch-to-buffer (get-buffer-create "**GHC Debug**"))
     (erase-buffer)
     (insert "Error loading ghc-mod-internal. Check that ghc-mod is available on the PATH.\n")
     (insert (format "Exception: %s\n" err))
     (insert (format "PATH: %s" (getenv "PATH")))))
  (when (fboundp 'ghc-mod-internal-debug)
    (ghc-mod-internal-debug)))

;;;###autoload
(defun ghc-init ()
  "Backwards compatible shim for ghc-mod-init"
  (ghc-mod-init))

;;;###autoload
(defun ghc-debug ()
  "Backwards compatible shim for ghc-mod-debug"
  (ghc-mod-debug))
