;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-process.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar  9, 2014

;;; Code:

(require 'ghc-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ghc-process-running nil)

(defvar-local ghc-process-process-name nil)
(defvar-local ghc-process-original-buffer nil)
(defvar-local ghc-process-original-file nil)
(defvar-local ghc-process-callback nil)
(defvar-local ghc-process-hook nil)

(defvar ghc-interactive-command "ghc-modi")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-get-project-root ()
  (ghc-run-ghc-mod '("root")))

(defun ghc-with-process (cmd callback &optional hook1 hook2)
  (unless ghc-process-process-name
    (setq ghc-process-process-name (ghc-get-project-root)))
  (when (and ghc-process-process-name (not ghc-process-running))
    (setq ghc-process-running t)
    (if hook1 (funcall hook1))
    (let* ((cbuf (current-buffer))
	   (name ghc-process-process-name)
	   (buf (get-buffer-create (concat " ghc-modi:" name)))
	   (file (buffer-file-name))
	   (cpro (get-process name)))
      (ghc-with-current-buffer buf
        (setq ghc-process-original-buffer cbuf)
	(setq ghc-process-original-file file)
	(setq ghc-process-callback callback)
	(setq ghc-process-hook hook2)
	(erase-buffer)
	(let ((pro (ghc-get-process cpro name buf)))
	  (process-send-string pro cmd)
	  (when ghc-debug
	    (ghc-with-debug-buffer
	     (insert (format "%% %s" cmd))))
	  pro)))))

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
  (let ((pro (start-file-process name buf ghc-interactive-command "-l" "-b" "\"\\n\"")))
    (set-process-filter pro 'ghc-process-filter)
    (set-process-sentinel pro 'ghc-process-sentinel)
    (set-process-query-on-exit-flag pro nil)
    pro))

(defun ghc-process-filter (process string)
  (let ((pbuf (process-buffer process)))
    (if (not (get-buffer pbuf))
	(setq ghc-process-running nil) ;; just in case
      (ghc-with-current-buffer (process-buffer process)
        (goto-char (point-max))
	(insert string)
	(forward-line -1)
	(cond
	 ((looking-at "^OK$")
	  (if ghc-process-hook (funcall ghc-process-hook))
	  (goto-char (point-min))
	  (funcall ghc-process-callback 'ok)
	  (when ghc-debug
	    (let ((cbuf (current-buffer)))
	      (ghc-with-debug-buffer
	       (insert-buffer-substring cbuf))))
	  (setq ghc-process-running nil))
	 ((looking-at "^NG ")
	  (funcall ghc-process-callback 'ng)
	  (when ghc-debug
	    (let ((cbuf (current-buffer)))
	      (ghc-with-debug-buffer
	       (insert-buffer-substring cbuf))))
	  (setq ghc-process-running nil)))))))

(defun ghc-process-sentinel (process event)
  (setq ghc-process-running nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ghc-process-rendezvous nil)
(defvar ghc-process-num-of-results nil)
(defvar ghc-process-results nil)

(defun ghc-sync-process (cmd &optional n hook)
  (unless ghc-process-running
    (setq ghc-process-rendezvous nil)
    (setq ghc-process-results nil)
    (setq ghc-process-num-of-results (or n 1))
    (let ((pro (ghc-with-process cmd 'ghc-process-callback nil hook)))
      (condition-case nil
	  (while (null ghc-process-rendezvous)
	    ;; 0.01 is too fast for Emacs 24.4.
	    ;; (sit-for 0.1 t) may get stuck when tooltip is displayed.
	    (sit-for 0.1)
	    ;; (discard-input) avoids getting stuck.
	    (discard-input))
	(quit
	 (setq ghc-process-running nil))))
    ghc-process-results))

(defun ghc-process-callback (status)
  (cond
   ((eq status 'ok)
    (let* ((n ghc-process-num-of-results)
	   (ret (if (= n 1)
		    (ghc-read-lisp-this-buffer)
		  (ghc-read-lisp-list-this-buffer n))))
      (setq ghc-process-results ret)))
   (t
    (setq ghc-process-results nil)))
  (setq ghc-process-num-of-results nil)
  (setq ghc-process-rendezvous t))

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
