;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-ins-mod.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Dec 27, 2011

;;; Code:

(defvar ghc-hoogle-command "hoogle")

(defvar ghc-hoogle-keywords '("data" "server" "combine" "convert" "test" "dump" "rank" "log"))

(defun ghc-insert-module ()
  (interactive)
  (if (not (ghc-which ghc-hoogle-command))
      (message "\"%s\" not found" ghc-hoogle-command)
    (let* ((expr0 (thing-at-point 'symbol))
	   (expr (ghc-read-expression expr0)))
      (if (member expr ghc-hoogle-keywords)
	  (message "\"%s\" not allowed" expr)
	(let ((mods (ghc-function-to-modules expr)))
	  (if (null mods)
	      (message "No module guessed")
	    (let* ((first (car mods))
		   (ini (cons first 0))
		   (mod (if (= (length mods) 1)
			    first
			  (completing-read "Module name: " mods nil t ini))))
	      (save-excursion
		(ghc-goto-module-position)
		(insert "import " mod "\n")))))))))

(defun ghc-goto-module-position ()
  (goto-char (point-max))
  (if (re-search-backward "^import" nil t)
      (forward-line)
    (if (re-search-backward "^module" nil t)
	(forward-line)
      (goto-char (point-min)))))

(defun ghc-function-to-modules (fn)
  (with-temp-buffer
    (call-process ghc-hoogle-command nil t nil fn)
    (goto-char (point-min))
    (let ((regex (concat "^\\([a-zA-Z0-9.]+\\) " fn " "))
	  ret)
      (while (re-search-forward regex nil t)
	(setq ret (cons (match-string 1) ret)))
      (nreverse ret))))

(provide 'ghc-ins-mod)
