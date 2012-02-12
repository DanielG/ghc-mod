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

(defun ghc-show-annot (&optional ask)
  (interactive "P")
  (if (not (ghc-which ghc-module-command))
      (message "%s not found" ghc-module-command)
    (let ((modname (ghc-find-module-name)))
      (if (not modname)
	  (message "module should be specified")
	(ghc-show-annot0 ask modname)))))

(defvar *annot-point* 0)
(defvar *annot-ix* 0)
(defvar *annot-ovl* (make-overlay 0 0))
(overlay-put *annot-ovl* 'face 'region)

(defun ghc-show-annot0 (ask modname)
  (let* ((pt (point))
         (ln (int-to-string (line-number-at-pos)))
         (cn (int-to-string (current-column)))
	 (cdir default-directory)
         (buf (current-buffer))
	 (file (buffer-name)))
    (if (= *annot-point* pt)
        (setq *annot-ix* (+ 1 *annot-ix*))
      (progn
        (setq *annot-point* pt)
        (setq *annot-ix* 0)))
    (save-excursion
      (with-temp-buffer
        (cd cdir)
        (apply 'call-process ghc-module-command nil t nil
               `(,@(ghc-make-ghc-options) "annot" ,file ,modname ,ln ,cn))
        (let* ((types (read (buffer-substring (point-min) (1- (point-max)))))
               (cix (mod *annot-ix* (length types)))
               (tinfo (nth cix types))
               (pos (nth 0 tinfo))
               (type (nth 1 tinfo))
               (left (ghc-get-pos buf (nth 0 pos) (nth 1 pos)))
               (right (ghc-get-pos buf (nth 2 pos) (nth 3 pos))))
          (move-overlay *annot-ovl* (- left 1) (- right 1) buf)
          (message type))))))

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
