;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-check.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar  9, 2014

;;; Code:

;; other files' errors should go to 0
;; ghc-flymake-display-errors -> line column
;; ghc-flymake-jump
;; flymake-goto-prev-error
;; flymake-goto-next-error
;; no need to save

(require 'ghc-func)

(defvar-local ghc-check-running nil)
(defvar-local ghc-check-process-name nil)
(defvar-local ghc-check-original-buffer nil)
(defvar-local ghc-check-original-file nil)

(defun ghc-check-get-process-name ()
  (let ((file (buffer-file-name)))
    (with-temp-buffer
      (ghc-call-process ghc-module-command nil t nil "debug" file)
      (goto-char (point-min))
      (when (re-search-forward "^Root directory: +\\(.*\\)$" nil t)
	(match-string-no-properties 1)))))

(defun ghc-check-syntax ()
  (unless ghc-check-process-name
    (setq ghc-check-process-name (ghc-check-get-process-name)))
  (if (null ghc-check-process-name)
      (message "Can't check")
    (let* ((cbuf (current-buffer))
	   (name ghc-check-process-name)
	   (buf (get-buffer-create (concat " ghc-modi:" name)))
	   (file (buffer-file-name))
	   (cpro (get-process name)))
      (with-current-buffer buf
	(unless ghc-check-running
	  (setq ghc-check-running t)
	  (setq ghc-check-original-buffer cbuf)
	  (setq ghc-check-original-file file)
	  (erase-buffer)
	  (let ((pro (ghc-check-get-process cpro name buf)))
	    (process-send-string pro (concat file "\n"))))))))

(defun ghc-check-get-process (cpro name buf)
  (cond
   ((not cpro)
    (ghc-check-start-process name buf))
   ((not (eq (process-status cpro) 'run))
    (delete-process cpro)
    (ghc-check-start-process name buf))
   (t cpro)))

(defun ghc-check-start-process (name buf)
  (let ((pro (start-file-process name buf "ghc-modi")))
    (set-process-filter pro 'ghc-check-process-filter)
    (set-process-sentinel pro 'ghc-check-process-sentinel)
    (set-process-query-on-exit-flag pro nil)
    pro))

(ghc-defstruct hilit-info file line col msg)

(defun ghc-check-process-filter (process string)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string)
    (forward-line -1)
    (cond
     ((looking-at "^NG$")
      (setq ghc-check-running nil)
      (message "An error happens"))
     ((looking-at "^OK$")
      (goto-char (point-min))
      (let ((regex "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): *\\(.+\\)")
	    info infos)
	(while (re-search-forward regex nil t)
	  (setq info (ghc-make-hilit-info
		      :file (match-string 1)
		      :line (string-to-number (match-string 2))
		      :col  (string-to-number (match-string 3))
		      :msg  (match-string 4)))
	  (setq infos (cons info infos)))
	(setq infos (nreverse infos))
	(cond
	 (infos
	  (ghc-check-highlight-original-buffer ghc-check-original-buffer infos))
	 (t
	  (with-current-buffer ghc-check-original-buffer
	    (remove-overlays (point-min) (point-max) 'ghc-check t))
	  ;; fixme no changes
	  ))
	(setq ghc-check-running nil))))))

(defun ghc-check-process-sentinel ()
  )

(defun ghc-check-highlight-original-buffer (buf infos)
  (with-current-buffer buf
    (remove-overlays (point-min) (point-max) 'ghc-check t)
    (save-excursion
      (goto-char (point-min))
      (dolist (info infos)
	(let ((line (ghc-hilit-info-get-line info))
	      (msg (ghc-hilit-info-get-msg info))
	      beg end ovl)
	  (goto-char (point-min))
	  (forward-line (1- line))
	  (while (eq (char-after) 32) (forward-char))
	  (setq beg (point))
	  (forward-line)
	  (setq end (1- (point)))
	  (setq ovl (make-overlay beg end))
	  (overlay-put ovl 'ghc-check t)
	  (overlay-put ovl 'ghc-msg msg) ;; should be list
	  (let ((face (if (string-match "^Error" msg)
			  'ghc-face-error
			'ghc-face-warn)))
	    (overlay-put ovl 'face face)))))))

;; stolen from flymake.el
(defface ghc-face-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :inherit error))
  "Face used for marking error lines."
  :group 'ghc)

(defface ghc-face-warn
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "DarkOrange"))
    (t
     :inherit warning))
  "Face used for marking warning lines."
  :group 'ghc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ghc-check-display-errors ()
  (interactive)
  (let* ((ovls (ghc-check-overlay-here))
	 (errs (mapcar (lambda (ovl) (overlay-get ovl 'ghc-msg)) ovls)))
    (if (null ovls)
	(message "No errors or warnings")
      (ghc-display
       nil
       (lambda ()
;;	 (insert title "\n\n")
	 (mapc (lambda (x) (insert x "\n")) errs))))))

(defun ghc-check-overlay-here ()
  (let ((ovls (overlays-at (point))))
    (ghc-filter (lambda (ovl) (overlay-get ovl 'ghc-check)) ovls)))

(provide 'ghc-check)
