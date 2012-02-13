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
  (let* ((expr0 (ghc-things-at-point))
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
  (let* ((expr0 (ghc-things-at-point))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; annot
;;;

(defvar ghc-annot-overlay nil)

(make-variable-buffer-local 'ghc-annot-overlay)

(defun ghc-annot-set-ix (n)
  (overlay-put ghc-annot-overlay 'ix n))

(defun ghc-annot-get-ix ()
  (overlay-get ghc-annot-overlay 'ix))

(defun ghc-annot-set-point (pos)
  (overlay-put ghc-annot-overlay 'pos pos))

(defun ghc-annot-get-point ()
  (overlay-get ghc-annot-overlay 'pos))

(defun ghc-annot-set-types (types)
  (overlay-put ghc-annot-overlay 'types types))

(defun ghc-annot-get-types ()
  (overlay-get ghc-annot-overlay 'types))

(defun ghc-annot-init ()
  (setq ghc-annot-overlay (make-overlay 0 0))
  (overlay-put ghc-annot-overlay 'face 'region)
  (ghc-annot-set-ix 0)
  (ghc-annot-set-point 0)
  (setq after-change-functions
	(cons 'ghc-delete-annot-ovl after-change-functions)))

(defun ghc-delete-annot-ovl (beg end len)
  (when (overlayp ghc-annot-overlay)
    (delete-overlay ghc-annot-overlay)))

(defun ghc-show-annot ()
  (interactive)
  (if (not (ghc-which ghc-module-command))
      (message "%s not found" ghc-module-command)
    (let ((modname (ghc-find-module-name)))
      (if (not modname)
	  (message "module should be specified")
	(ghc-show-annot0 modname)))))

(defun ghc-show-annot0 (modname)
  (let* ((buf (current-buffer))
	 (types (ghc-get-annot modname))
	 (tinfo (nth (ghc-annot-get-ix) types))
	 (pos (nth 0 tinfo))
	 (type (nth 1 tinfo))
	 (left (ghc-get-pos buf (nth 0 pos) (nth 1 pos)))
	 (right (ghc-get-pos buf (nth 2 pos) (nth 3 pos))))
      (move-overlay ghc-annot-overlay (- left 1) (- right 1) buf)
      (message type)))

(defun ghc-get-annot (modname)
  (if (= (ghc-annot-get-point) (point))
      (ghc-annot-set-ix
       (mod (1+ (ghc-annot-get-ix)) (length (ghc-annot-get-types))))
    (ghc-annot-set-types (ghc-call-annot modname))
    (ghc-annot-set-point (point))
    (ghc-annot-set-ix 0))
  (ghc-annot-get-types))

(defun ghc-call-annot (modname)
  (let* ((ln (int-to-string (line-number-at-pos)))
	 (cn (int-to-string (current-column)))
	 (cdir default-directory)
	 (file (buffer-name)))
    (ghc-read-lisp
     (lambda ()
       (cd cdir)
       (apply 'call-process ghc-module-command nil t nil
	      `(,@(ghc-make-ghc-options) "annot" ,file ,modname ,ln ,cn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Misc
;;;

(defun ghc-get-pos (buf line col)
  (save-excursion
    (set-buffer buf)
    (goto-line line)
    (forward-char col)
    (point)))

(defun ghc-read-expression (default)
  (if default
      (let ((prompt (format "Expression (%s): " default)))
	(read-string prompt default nil))
    (read-string "Expression: ")))

(defun ghc-find-module-name ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^module[ ]+\\([^ ]+\\)" nil t)
	(match-string-no-properties 1))))

(provide 'ghc-info)
