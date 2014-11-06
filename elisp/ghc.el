;;; ghc.el --- ghc-mod front-end for haskell-mode   -*- coding: utf-8-emacs; -*-

;; Copyright (C) 2009-2014  Kazu Yamamoto, Daniel Gröber

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;;          Daniel Gröber <dxld@darkboxed.org>
;; URL: https://github.com/kazu-yamamoto/
;; Created: Sep 25, 2009
;; Revised: Aug 13, 2014
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))


;;; Commentary:

;; Installation:
;;
;; To install ghc-mod-mode put the elisp/ directory of the ghc-mod source
;; distribution somewhere on your `load-path'. If you installed ghc-mod through
;; ELPA/package.el or a distribution package this will already be taken care
;; of. Please note that installation through ELPA/package.el is discouraged
;; since `ghc-mod-mode' depends on the "ghc-mod" executable which is not
;; distributed as part of the ELPA package and installing if from another source
;; could lead to version incompatibilities between `ghc-mod-mode' and the
;; executable.


;; Initialization:
;;
;; To initialize `ghc-mod-mode' you should either call `global-ghc-mod-mode' to
;; have `ghc-mod-mode' enabled in all `haskell-mode' buffers automatically or
;; type `M-x ghc-mod-mode RET' whenever you want to enable it manually.
;;
;; (autoload 'ghc-mod-mode "ghc" nil t)
;; (autoload 'ghc-mod-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-mod-mode)))
;;

;; TODO?:
;; Or if you wish to display error each goto next/prev error,
;; set ghc-display-error valiable.
;;
;; (setq ghc-display-error 'minibuffer) ; to minibuffer
;; ; (setq ghc-display-error 'other-buffer) ; to other-buffer

;;

;;; Code:

;; defvar-local was introduced in 24.3
(let* ((major 24)
       (minor 3))
  (if (or (< emacs-major-version major)
	  (and (= emacs-major-version major)
	       (< emacs-minor-version minor)))
      (error "ghc-mod requires at least Emacs %d.%d" major minor)))

(defconst ghc-version "0")

;; (eval-when-compile
;;  (require 'haskell-mode))

(require 'ghc-comp)
(require 'ghc-doc)
(require 'ghc-info)
(require 'ghc-check)
(require 'ghc-command)
(require 'ghc-ins-mod)
(require 'ghc-indent)
(require 'ghc-rewrite)
(require 'dabbrev)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Deprecated customization variables
;;;

(defun ghc-find-C-h ()
  (or
   (when keyboard-translate-table
     (aref keyboard-translate-table ?\C-h))
   ?\C-h))

(defvar ghc-mod-key-varaibles-depricated-notice
  "Since ghc-mod 5.1.0.0 using ghc-*-key variables to change
keybindings is deprecated, setting them will be ignored. Use the
`define-key' function to change `ghc-mod-mode-map' in a
`ghc-mod-mode-hook' instead.")

(eval
 (let ((msg-var 'ghc-mod-key-varaibles-depricated-notice))
   `(progn
      (defvar ghc-completion-key  "\e\t" ,msg-var)
      (defvar ghc-document-key    "\e\C-d" ,msg-var)
      (defvar ghc-import-key      "\e\C-m" ,msg-var)
      (defvar ghc-previous-key    "\ep" ,msg-var)
      (defvar ghc-next-key        "\en" ,msg-var)
      (defvar ghc-help-key        "\e?" ,msg-var)
      (defvar ghc-insert-key      "\et" ,msg-var)
      (defvar ghc-sort-key        "\es" ,msg-var)
      (defvar ghc-type-key        "\C-c\C-t" ,msg-var)
      (defvar ghc-info-key        "\C-c\C-i" ,msg-var)
      (defvar ghc-toggle-key      "\C-c\C-c" ,msg-var)
      (defvar ghc-jump-key        "\C-c\C-j" ,msg-var)
      (defvar ghc-module-key      "\C-c\C-m" ,msg-var)
      (defvar ghc-expand-key      "\C-c\C-e" ,msg-var)
      (defvar ghc-kill-key        "\C-c\C-k" ,msg-var)
      (defvar ghc-hoogle-key      (format "\C-c%c" (ghc-find-C-h)) ,msg-var)
      (defvar ghc-shallower-key   "\C-c<" ,msg-var)
      (defvar ghc-deeper-key      "\C-c>" ,msg-var)
      ;;(defvar ghc-case-split-key  "\C-c\C-s"  ,msg-var)
      (defvar ghc-refine-key      "\C-c\C-f" ,msg-var)
      (defvar ghc-auto-key        "\C-c\C-a" ,msg-var)
      (defvar ghc-prev-hole-key   "\C-c\ep" ,msg-var)
      (defvar ghc-next-hole-key   "\C-c\en" ,msg-var)
      )))

(defvar ghc-mod-depricated-key-vars
  (let ((vs '(completion document import previous next help insert sort type
             info toggle jump module expand kill hoogle shallower deeper refine
             auto prev-hole next-hole)))
    (mapcar (lambda (n) (intern (concat "ghc-" (symbol-name n) "-key"))) vs)) )

(defun ghc-mod-depricated-key-values ()
  (mapcar (lambda (s) (symbol-value s)) ghc-mod-depricated-key-vars) )

(defvar ghc-mod-initial-key-values (ghc-mod-depricated-key-values) )
(defvar ghc-mod-key-vars-depricated-warning-was-displayed nil)

(defun ghc-mod-display-key-vars-deprecated-warning-if-appropriate ()
  (when (not (equal
              (ghc-mod-depricated-key-values)
              ghc-mod-initial-key-values))
    (unless ghc-mod-key-vars-depricated-warning-was-displayed
      (let ((buf (get-buffer-create "*ghc-mod warnings*")))
        (with-current-buffer buf
          (insert "Warning: ")
          (insert ghc-mod-key-varaibles-depricated-notice)
          (insert "\n\nYou are getting this warning because:\n\n")
          (insert (ghc-mod-key-vars-explain) "\n")
          (setq buffer-read-only t))
        (display-buffer buf)
        (setq ghc-mod-key-vars-depricated-warning-was-displayed t)))))


(defun ghc-mod-key-vars-explain ()
  "Explain which variable is triggering the deprecation warning
  for the user."
  (mapconcat 'identity
             (loop
              for key     in ghc-mod-depricated-key-vars
              for initial in ghc-mod-initial-key-values
              for current in (ghc-mod-depricated-key-values)
              when (not (equal initial current))
              collect
              (format " - %s is set to \"%s\" instead of its default value"
                      (symbol-name key) current))
             "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; global ghc-mod Minor mode
;;;

(define-globalized-minor-mode global-ghc-mod-mode
  ghc-mod-mode
  (lambda () (when (eq major-mode 'haskell-mode) (ghc-mod-mode 1)))
  turn-on-ghc-mod-mode)

;; (defmacro with-all-haskell-mode-buffers (&rest body)
;;   `(loop for b in (buffer-list) do
;;          (with-current-buffer b
;;            (when (eq major-mode 'haskell-mode)
;;              ,@body)
;;            )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc-mod Minor mode
;;;

(defvar ghc-mod-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "ESC t")      'ghc-complete)
    (define-key keymap (kbd "ESC C-d")    'ghc-browse-document)
    (define-key keymap (kbd "C-c C-t")    'ghc-show-type)
    (define-key keymap (kbd "C-c C-i")    'ghc-show-info)
    (define-key keymap (kbd "C-c C-e")    'ghc-expand-th)
    (define-key keymap (kbd "ESC C-m")    'ghc-import-module)
    (define-key keymap (kbd "ESC p")      'ghc-goto-prev-error)
    (define-key keymap (kbd "ESC n")      'ghc-goto-next-error)
    (define-key keymap (kbd "ESC ?")      'ghc-display-errors)
    (define-key keymap (kbd "ESC t")      'ghc-mod-insert-template-or-signature)
    (define-key keymap (kbd "ESC s")      'ghc-sort-lines)
    (define-key keymap (kbd "C-x C-s")    'ghc-save-buffer)
    (define-key keymap (kbd "C-c C-c")    'ghc-toggle-check-command)
    (define-key keymap (kbd "C-c C-j")    'ghc-jump-file)
    (define-key keymap (kbd "C-c C-m")    'ghc-insert-module)
    (define-key keymap (kbd "C-c C-k")    'ghc-kill-process)
    (define-key keymap (format "\C-c%c" (ghc-find-C-h))  'haskell-hoogle)
    (define-key keymap (kbd "C-c <")      'ghc-make-indent-shallower)
    (define-key keymap (kbd "C-c >")      'ghc-make-indent-deeper)
    ;(define-key keymap (kbd "C-c C-f")   'ghc-case-split)
    (define-key keymap (kbd "C-c C-f")    'ghc-refine)
    (define-key keymap (kbd "C-c C-a")    'ghc-auto)
    (define-key keymap (kbd "C-c ESC p")  'ghc-goto-prev-hole)
    (define-key keymap (kbd "C-c ESC n")  'ghc-goto-next-hole)
    keymap))

(defadvice save-buffer (after ghc-mod-check-syntax-on-save disable)
    "Check syntax with GHC when a haskell-mode buffer is saved."
    (when ghc-mod-mode
      (ghc-check-syntax)))

(defun ghc-mod-init ()
  (ghc-mod-display-key-vars-deprecated-warning-if-appropriate)
  (ghc-abbrev-init)
  (ghc-type-init)
  (ghc-comp-init)
  (ghc-check-syntax)
  (ad-enable-advice 'save-buffer 'after 'ghc-mod-check-syntax-on-save)
  )

(defun ghc-mod-deinit ()
  (ad-disable-advice 'save-buffer 'after 'ghc-mod-check-syntax-on-save)
  (ghc-comp-deinit)
  (ghc-type-deinit)
  (ghc-abbrev-deinit)
  )

(defvar ghc-mod-default-lighter " Gᷟ")
(defvar-local ghc-mod-lighter ghc-mod-default-lighter)

(defun ghc-abbrev-init ()
  (set (make-local-variable 'dabbrev-case-fold-search) nil))

(defun ghc-abbrev-deinit ()
  (kill-local-variable 'dabbrev-case-fold-search) )

;;;###autoload
(define-minor-mode ghc-mod-mode
  "Toggle ghc-mod mode on or off.
With a prefix argument ARG, enable ghc-mod mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'."
  :init-value nil
  :lighter ghc-mod-lighter
  :keymap ghc-mod-mode-map
  :group 'ghc-mod

  (if ghc-mod-mode
      (ghc-mod-init)
    (ghc-mod-deinit)))

;;;###autoload
(defun ghc-init ()
  "Deprecated: use `ghc-mod-mode' instead."
  (unless ghc-mod-mode (ghc-mod-mode)))

;;;###autoload
(defun ghc-mod-debug ()
  (interactive)
  (let ((el-path (locate-file "ghc.el" load-path))
	(ghc-path (executable-find "ghc")) ;; FIXME
	(ghc-mod-path (executable-find ghc-module-command))
	(ghc-modi-path (executable-find ghc-interactive-command))
	(el-ver ghc-version)
	(ghc-ver (ghc-run-ghc-mod '("--version") "ghc"))
	(ghc-mod-ver (ghc-run-ghc-mod '("version")))
	(ghc-modi-ver (ghc-run-ghc-mod '("version") ghc-interactive-command))
	(path (getenv "PATH")))
    (switch-to-buffer (get-buffer-create "**GHC Debug**"))
    (erase-buffer)
    (insert "Path: check if you are using intended programs.\n")
    (insert (format "\t  ghc.el path: %s\n" el-path))
    (insert (format "\t ghc-mod path: %s\n" ghc-mod-path))
    (insert (format "\tghc-modi path: %s\n" ghc-modi-path))
    (insert (format "\t     ghc path: %s\n" ghc-path))
    (insert "\nVersion: all versions must be the same.\n")
    (insert (format "\t  ghc.el version %s\n" el-ver))
    (insert (format "\t %s\n" ghc-mod-ver))
    (insert (format "\t%s\n" ghc-modi-ver))
    (insert (format "\t%s\n" ghc-ver))
    (insert "\nEnvironment variables:\n")
    (insert (format "\tPATH=%s\n" path))))

(defun ghc-insert-template-or-signature (&optional flag)
  (interactive "P")
  (if flag
      (ghc-initial-code-from-signature)
    (ghc-insert-template)))

(provide 'ghc)
;;; ghc.el ends here
