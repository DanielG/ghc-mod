;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-func.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 25, 2009

;;; Code:

(defvar ghc-module-command "ghc-mod"
"*The command name of \"ghc-mod\"")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-replace-character (string from to)
  "Replace characters equal to FROM to TO in STRING."
  (let ((ret (copy-sequence string)))
    (dotimes (cnt (length ret) ret)
      (if (char-equal (aref ret cnt) from)
	  (aset ret cnt to)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ghc-add (sym val)
  `(setq ,sym (cons ,val ,sym)))

(defun ghc-set (vars vals)
  (dolist (var vars)
    (if var (set var (car vals))) ;; var can be nil to skip
    (setq vals (cdr vals))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-filter (pred lst)
  (let (ret)
    (dolist (x lst (reverse ret))
      (if (funcall pred x) (ghc-add ret x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-which (cmd)
  (catch 'loop
    (dolist (suffix '("" ".exe"))
      (let ((cmds (concat cmd suffix)))
	(dolist (dir exec-path)
	  (let ((path (expand-file-name cmds dir)))
	    (if (file-exists-p path)
		(throw 'loop path))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-uniq-lol (lol)
  (let ((hash (make-hash-table :test 'equal))
	ret)
    (dolist (lst lol)
      (dolist (key lst)
	(puthash key key hash)))
    (maphash (lambda (key val) (ghc-add ret key)) hash)
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-read-lisp (func)
  (with-temp-buffer
    (funcall func)
    (goto-char (point-min))
    (condition-case nil
	(read (current-buffer))
      (error ()))))

(defun ghc-read-lisp-list (func n)
  (with-temp-buffer
    (funcall func)
    (goto-char (point-min))
    (condition-case nil
	(let ((m (set-marker (make-marker) 1 (current-buffer)))
	      ret)
	  (dotimes (i n (nreverse ret))
	    (ghc-add ret (read m))))
      (error ()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-mapconcat (func list)
  (apply 'append (mapcar func list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ghc-null 0)
(defconst ghc-newline 10)

(provide 'ghc-func)
