;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-process.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar  9, 2014

;;; Code:

(require 'ghc-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local ghc-process-running nil)
(defvar-local ghc-process-process-name nil)
(defvar-local ghc-process-original-buffer nil)
(defvar-local ghc-process-original-file nil)
(defvar-local ghc-process-callback nil)

(defvar ghc-interactive-command "ghc-modi")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-get-project-root ()
  (let ((file (buffer-file-name)))
    (when file
      (with-temp-buffer
	(ghc-call-process ghc-module-command nil t nil "root" file)
	(goto-char (point-min))
	(when (looking-at "^\\(.*\\)$")
	  (match-string-no-properties 1))))))

(defun ghc-with-process (send callback)
  (unless ghc-process-process-name
    (setq ghc-process-process-name (ghc-get-project-root)))
  (when ghc-process-process-name
    (let* ((cbuf (current-buffer))
	   (name ghc-process-process-name)
	   (buf (get-buffer-create (concat " ghc-modi:" name)))
	   (file (buffer-file-name))
	   (cpro (get-process name)))
      (with-current-buffer buf
	(unless ghc-process-running
	  (setq ghc-process-running t)
	  (setq ghc-process-original-buffer cbuf)
	  (setq ghc-process-original-file file)
	  (setq ghc-process-callback callback)
	  (erase-buffer)
	  (let ((pro (ghc-get-process cpro name buf))
		(cmd (funcall send)))
	    (process-send-string pro cmd)
	    (when ghc-debug
	      (ghc-with-debug-buffer
	       (insert (format "%% %s" cmd))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-get-process (cpro name buf)
  (cond
   ((not cpro)
    (ghc-start-process name buf))
   ((not (eq (process-status cpro) 'run))
    (delete-process cpro)
    (ghc-start-process name buf))
   (t cpro)))

(defun ghc-start-process (name buf)
  (let ((pro (start-file-process name buf ghc-interactive-command)))
    (set-process-filter pro 'ghc-process-filter)
    (set-process-query-on-exit-flag pro nil)
    pro))

(defun ghc-process-filter (process string)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string)
    (forward-line -1)
    (when (looking-at "^\\(OK\\|NG\\)$")
      (goto-char (point-min))
      (funcall ghc-process-callback)
      (when ghc-debug
	(let ((cbuf (current-buffer)))
	  (ghc-with-debug-buffer
	   (insert-buffer-substring cbuf))))
      (setq ghc-process-running nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-kill-process ()
  (interactive)
  (let* ((name ghc-process-process-name)
	 (cpro (if name (get-process name))))
    (if (not cpro)
	(message "No process")
      (delete-process cpro)
      (message "A process was killed"))))

(provide 'ghc-process)
