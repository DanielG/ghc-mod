;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-types.el
;;;

;; Author:  Daniel Schoepe <daniel.schoepe@googlemail.com>
;; Created: Oct 29, 2010

;;; Code:

(require 'ghc-func)
(require 'ghc-comp)

(defvar ghc-last-point-pos 0 "Position of point during last idle-timer run")
(defvar ghc-show-type-delay 1
  "Time (in seconds) until type for symbol under point is shown")
(defvar ghc-show-type-timer nil)

(defun ghc-get-type (str)
  (let* ((mods (ghc-gather-import-modules-buffer))
         (response (with-temp-buffer
                     (call-process ghc-module-command nil t nil "type" str
                                   (prin1-to-string mods))
                     (buffer-substring-no-properties (point-min) (point-max)))))
    (if (and response (not (string= response "")))
        response)))

(defun ghc-get-qualification ()
  (let ((prefix nil))
    (save-excursion
      (while (looking-back "\\([A-Za-z_][A-Za-z_'0-9#]*\\.\\)" nil t)
        (setq prefix (concat (match-string-no-properties 1) prefix))
        (goto-char (match-beginning 1)))
      prefix)))

(defun ghc-get-type-under-point ()
  "Tries to determine the type of the symbol under the point"
  (let ((mods (ghc-gather-import-modules-buffer))
        (cur-word (thing-at-point 'word)))
    (if cur-word
        (save-excursion
          (if (not (looking-back "[\\. ]"))
              (backward-word))
          (let ((prefix (ghc-get-qualification)))
            (if (and prefix
                     (member (substring prefix 0 -1)
                             (apply 'append (mapcar 'ghc-module-get-qualified
                                                    mods))))
                (ghc-get-type (concat prefix cur-word))
              (ghc-get-type cur-word)))))))

(defun ghc-show-type-under-point ()
  "Displays type under point in the echo area"
  (let ((type (ghc-get-type-under-point)))
    (if type
        (message (propertize type 'face 'bold)))))

(defun ghc-show-timer-func ()
  (if (and (eq major-mode 'haskell-mode)
           (not (eq (point) ghc-last-point-pos))
           (not (eq (selected-window) (minibuffer-window))))
      (progn
        (ghc-show-type-under-point)
        (setq ghc-last-point-pos (point)))))

(defun ghc-turn-on-show-type ()
  "Turn on displaying the type of the word under the point"
  (interactive)
  (if (timerp ghc-show-type-timer)
      (cancel-timer ghc-show-type-timer))
  (setq ghc-show-type-timer (run-with-idle-timer ghc-show-type-delay t
                                                 'ghc-show-timer-func))
  (message "Automatic type display on"))

(defun ghc-turn-off-show-type ()
  "Turn off displaying the type of the word under the point"
  (interactive)
  (cancel-timer ghc-show-type-timer)
  (setq ghc-show-type-timer nil)
  (message "Automatic type display off"))

(defun ghc-toggle-show-type ()
  "Toggles whether or not to show the type of the symbol under the point"
  (interactive)
  (if ghc-show-type-timer
      (ghc-turn-off-show-type)
    (ghc-turn-on-show-type)))

(provide 'ghc-types)
