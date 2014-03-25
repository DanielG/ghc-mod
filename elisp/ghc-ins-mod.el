;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-ins-mod.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Dec 27, 2011

;;; Code:

(defvar ghc-ins-mod-rendezvous nil)

(defun ghc-insert-module ()
  (interactive)
  (let* ((expr0 (ghc-things-at-point))
	 (expr (ghc-read-expression expr0)))
    (let ((mods (ghc-function-to-modules expr)))
      (if (null mods)
	  (message "No module guessed")
	(let ((mod (ghc-completing-read "Module name (%s): " mods)))
	  (save-excursion
	    (ghc-goto-module-position)
	    (if (string-match "^[a-zA-Z0-9_]$" expr)
		(insert "import " mod " (" expr ")\n")
	      (insert "import " mod " ((" expr "))\n"))))))))

(defun ghc-completing-read (fmt lst)
  (let* ((def (car lst))
	 (prompt (format fmt def))
	 (inp (completing-read prompt lst)))
    (if (string= inp "") def inp)))

(defun ghc-goto-module-position ()
  (goto-char (point-max))
  (if (re-search-backward "^import" nil t)
      (ghc-goto-empty-line)
    (if (re-search-backward "^module" nil t)
	(ghc-goto-empty-line)
      (goto-char (point-min)))))

(defun ghc-goto-empty-line ()
  (unless (re-search-forward "^$" nil t)
    (forward-line)))

(defun ghc-function-to-modules (fun)
  (setq ghc-ins-mod-rendezvous nil)
  (ghc-with-process
   (lambda () (ghc-ins-mod-send fun))
   'ghc-ins-mod-callback)
  (while (null ghc-ins-mod-rendezvous)
    (sit-for 0.01))
  ghc-ins-mod-rendezvous)

(defun ghc-ins-mod-send (fun)
  (concat "find " fun "\n"))

(defun ghc-ins-mod-callback ()
  (let (lines line beg)
    (while (not (eobp))
      (setq beg (point))
      (forward-line)
      (setq line (buffer-substring-no-properties beg (1- (point))))
      (setq lines (cons line lines)))
    (with-current-buffer ghc-process-original-buffer
      (setq ghc-ins-mod-rendezvous
	    (nreverse (cdr lines)))))) ;; removing "OK"

(provide 'ghc-ins-mod)
