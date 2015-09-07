;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-process.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar  9, 2014

;;; Code:

(require 'ghc-func)

(defvar ghc-debug-options nil)
;; (setq ghc-debug-options '("-v9"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ghc-process-running nil)
(defvar ghc-process-file-mapping nil)

(defvar-local ghc-process-process-name nil)
(defvar-local ghc-process-original-buffer nil)
(defvar-local ghc-process-original-file nil)
(defvar-local ghc-process-callback nil)
(defvar-local ghc-process-hook nil)
(defvar-local ghc-process-root nil)

(defvar ghc-command "ghc-mod")

(defvar ghc-error-buffer "*GHC Error*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-get-project-root ()
  (ghc-run-ghc-mod '("root")))

(defun ghc-with-process (cmd callback &optional hook1 hook2)
  (let ((root (ghc-get-project-root)))
    (unless ghc-process-process-name
      (setq ghc-process-process-name root))
    (when (and ghc-process-process-name (not ghc-process-running))
      (setq ghc-process-running t)
      (if hook1 (funcall hook1))
      (let* ((cbuf (current-buffer))
	     (name ghc-process-process-name)
	     (buf (get-buffer-create (concat " ghc-mod:" name)))
	     (file (buffer-file-name))
	     (cpro (get-process name)))
	(ghc-with-current-buffer buf
	  (setq ghc-process-original-buffer cbuf)
	  (setq ghc-process-original-file file)
	  (setq ghc-process-hook hook2)
	  (setq ghc-process-root root)
	  (let ((pro (ghc-get-process cpro name buf))
		(map-cmd (format "map-file %s\n" file)))
	    ;; map-file
	    (setq ghc-process-file-mapping t)
	    (setq ghc-process-callback nil)
	    (erase-buffer)
	    (when ghc-debug
	      (ghc-with-debug-buffer
	       (insert (format "%% %s" map-cmd))
	       (insert "CONTENTS + EOT\n")))
	    (process-send-string pro map-cmd)
	    (with-current-buffer cbuf
	      (save-restriction
		(widen)
		(process-send-region pro (point-min) (point-max))))
	    (process-send-string pro "\004\n")
	    (condition-case nil
		(let ((inhibit-quit nil))
		  (while ghc-process-file-mapping
		    (accept-process-output pro 0.1 nil t)))
	      (quit
	       (setq ghc-process-running nil)
	       (setq ghc-process-file-mapping nil)))
	    ;; command
	    (setq ghc-process-callback callback)
	    (erase-buffer)
	    (when ghc-debug
	      (ghc-with-debug-buffer
	       (insert (format "%% %s" cmd))))
	    (process-send-string pro cmd)
	    pro))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-get-process (cpro name buf)
  (cond
   ((not cpro)
    (ghc-start-process name buf))
   ((not (eq (process-status cpro) 'run))
    (delete-process cpro)
    (ghc-start-process name buf))
   (t cpro)))

(defvar ghc-process-wrapper-function #'identity)

(defun ghc-start-process (name buf)
  (let* ((process-connection-type nil) ;; using PIPE due to ^D
	 (opts (append ghc-debug-options
		       '("-b" "\n" "-l" "--line-prefix=O: ,E: ")
		       (ghc-make-ghc-options)
		       '("legacy-interactive")))
	 (command (funcall ghc-process-wrapper-function (append (list ghc-command) opts)))
	 (pro (apply 'start-process name buf (car command) (cdr command))))
    (set-process-filter pro 'ghc-process-filter)
    (set-process-sentinel pro 'ghc-process-sentinel)
    (set-process-query-on-exit-flag pro nil)
    pro))

(defun ghc-process-filter (process string)
  (let* ((pbuf (process-buffer process))
	 (tbufname (concat " tmp " (buffer-name pbuf)))
	 tbuf)
    (if (not (get-buffer pbuf))
	(setq ghc-process-running nil) ;; just in case
      (ghc-with-current-buffer pbuf
	(when ghc-debug
	  (ghc-with-debug-buffer
	   (insert string)))
	(with-current-buffer (get-buffer-create tbufname)
	  (setq tbuf (current-buffer))
	  (goto-char (point-max))
	  (insert string)
	  (goto-char (point-min))
	  (let ((cont t) end out)
	    (while (and cont (not (eobp)))
	      (cond
	       ((looking-at "^O: ")
		(setq out t))
	       ((looking-at "^E: ")
		(setq out nil))
	       (t
		(setq cont nil)))
	      (when cont
		(forward-line)
		(unless (bolp) (setq cont nil)))
	      (when cont
		(delete-region 1 4)
		(setq end (point))
		(if out
		    (with-current-buffer pbuf
		      (goto-char (point-max))
		      (insert-buffer-substring tbuf 1 end))
		  (with-current-buffer (get-buffer-create ghc-error-buffer)
		    (setq buffer-read-only t)
		    (let* ((buffer-read-only nil)
			   (inhibit-read-only t)
			   (cbuf (current-buffer))
			   cwin)
		      (unless (get-buffer-window cbuf) (display-buffer cbuf))
		      (setq cwin (get-buffer-window cbuf))
		      (with-selected-window cwin
			(goto-char (point-max))
			(insert-buffer-substring tbuf 1 end)
			(set-buffer-modified-p nil))
		      (redisplay))))
		(delete-region 1 end)))))
	(goto-char (point-max))
	(forward-line -1)
	(cond
	 ((looking-at "^OK$")
	  (delete-region (point) (point-max))
	  (setq ghc-process-file-mapping nil)
	  (when ghc-process-callback
	    (if ghc-process-hook (funcall ghc-process-hook))
	    (goto-char (point-min))
	    (funcall ghc-process-callback 'ok)
	    (setq ghc-process-running nil)))
	 ((looking-at "^NG ")
	  (funcall ghc-process-callback 'ng)
	  (setq ghc-process-running nil)))))))

(defun ghc-process-sentinel (_process _event)
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
      ;; ghc-process-running is now t.
      ;; But if the process exits abnormally, it is set to nil.
      (condition-case nil
	  (let ((inhibit-quit nil))
	    (while (and (null ghc-process-rendezvous) ghc-process-running)
	      (accept-process-output pro 0.1 nil t)))
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
