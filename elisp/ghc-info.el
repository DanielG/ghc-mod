;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-info.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Nov 15, 2010

;;; Code:

(require 'ghc-func)

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
	 (file buffer-file-name)
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
;;; type
;;;

(defvar ghc-type-overlay nil)

(make-variable-buffer-local 'ghc-type-overlay)

(defun ghc-type-set-ix (n)
  (overlay-put ghc-type-overlay 'ix n))

(defun ghc-type-get-ix ()
  (overlay-get ghc-type-overlay 'ix))

(defun ghc-type-set-point (pos)
  (overlay-put ghc-type-overlay 'pos pos))

(defun ghc-type-get-point ()
  (overlay-get ghc-type-overlay 'pos))

(defun ghc-type-set-types (types)
  (overlay-put ghc-type-overlay 'types types))

(defun ghc-type-get-types ()
  (overlay-get ghc-type-overlay 'types))

(ghc-defstruct tinfo beg-line beg-column end-line end-column info)

(defun ghc-type-init ()
  (setq ghc-type-overlay (make-overlay 0 0))
  (overlay-put ghc-type-overlay 'face 'region)
  (ghc-type-set-ix 0)
  (ghc-type-set-point 0)
  (setq after-change-functions
	(cons 'ghc-type-delete-overlay after-change-functions))
  (set (make-local-variable 'post-command-hook) 'ghc-type-post-command-hook))

(defun ghc-type-delete-overlay (&optional beg end len)
  (when (overlayp ghc-type-overlay)
    (delete-overlay ghc-type-overlay)))

(defun ghc-type-post-command-hook ()
  (when (and (overlayp ghc-type-overlay)
	     (/= (ghc-type-get-point) (point)))
    (ghc-type-delete-overlay)))

(defun ghc-show-type ()
  (interactive)
  (if (not (ghc-which ghc-module-command))
      (message "%s not found" ghc-module-command)
    (let ((modname (ghc-find-module-name)))
      (if (not modname)
	  (message "module should be specified")
	(ghc-show-type0 modname)))))

(defun ghc-show-type0 (modname)
  (let* ((buf (current-buffer))
	 (tinfos (ghc-type-get-tinfos modname)))
    (if (null tinfos)
	(message "Cannot guess type")
      (let* ((tinfo (nth (ghc-type-get-ix) tinfos))
	     (type (ghc-tinfo-get-info tinfo))
	     (beg-line (ghc-tinfo-get-beg-line tinfo))
	     (beg-column (ghc-tinfo-get-beg-column tinfo))
	     (end-line (ghc-tinfo-get-end-line tinfo))
	     (end-column (ghc-tinfo-get-end-column tinfo))
	     (left (ghc-get-pos buf beg-line beg-column))
	     (right (ghc-get-pos buf end-line end-column)))
	(move-overlay ghc-type-overlay (- left 1) (- right 1) buf)
	(message type)))))

(defun ghc-type-get-tinfos (modname)
  (if (= (ghc-type-get-point) (point))
      (ghc-type-set-ix
       (mod (1+ (ghc-type-get-ix)) (length (ghc-type-get-types))))
    (ghc-type-set-types (ghc-type-obtain-tinfos modname))
    (ghc-type-set-point (point))
    (ghc-type-set-ix 0))
  (ghc-type-get-types))

(defun ghc-type-obtain-tinfos (modname)
  (let* ((ln (int-to-string (line-number-at-pos)))
	 (cn (int-to-string (current-column)))
	 (cdir default-directory)
	 (file buffer-file-name))
    (ghc-read-lisp
     (lambda ()
       (cd cdir)
       (apply 'call-process ghc-module-command nil t nil
	      `(,@(ghc-make-ghc-options) "-l" "type" ,file ,modname ,ln ,cn))
       (goto-char (point-min))
       (while (search-forward "[Char]" nil t)
	 (replace-match "String"))))))

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
