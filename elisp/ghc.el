;;; ghc.el --- ghc-mod front-end for haskell-mode

;; Copyright (C) 2009-2014  Kazu Yamamoto, Daniel Gröber
;; Author:  Kazu Yamamoto <Kazu@Mew.org>, Daniel Gröber <dxld@darkboxed.org>
;; Created: Sep 25, 2009
;; Revised: Aug 13, 2014

;; Put the following code to your "~/.emacs".
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
;;; Customize Variables
;;;

(defun ghc-find-C-h ()
  (or
   (when keyboard-translate-table
     (aref keyboard-translate-table ?\C-h))
   ?\C-h))

(defvar ghc-mod-key-varaibles-depricated-notice
  "Using ghc-*-key variables to change keybindings is deprecated. Use TODO TODO instead. ")

(defvar ghc-completion-key  "\e\t")
(defvar ghc-document-key    "\e\C-d")
(defvar ghc-import-key      "\e\C-m")
(defvar ghc-previous-key    "\ep")
(defvar ghc-next-key        "\en")
(defvar ghc-help-key        "\e?")
(defvar ghc-insert-key      "\et")
(defvar ghc-sort-key        "\es")
(defvar ghc-type-key        "\C-c\C-t")
(defvar ghc-info-key        "\C-c\C-i")
(defvar ghc-toggle-key      "\C-c\C-c")
(defvar ghc-jump-key        "\C-c\C-j")
(defvar ghc-module-key      "\C-c\C-m")
(defvar ghc-expand-key      "\C-c\C-e")
(defvar ghc-kill-key        "\C-c\C-k")
(defvar ghc-hoogle-key      (format "\C-c%c" (ghc-find-C-h)))
(defvar ghc-shallower-key   "\C-c<")
(defvar ghc-deeper-key      "\C-c>")
;(defvar ghc-case-split-key  "\C-c\C-s")
(defvar ghc-refine-key      "\C-c\C-f")
(defvar ghc-auto-key        "\C-c\C-a")
(defvar ghc-prev-hole-key   "\C-c\ep")
(defvar ghc-next-hole-key   "\C-c\en")

(defvar ghc-mod-depricated-key-vars
  (let ((vs '(completion document import previous next help insert sort type
             info toggle jump module expand kill hoogle shallower deeper refine
             auto prev-hole next-hole)))
    (mapcar (lambda (n) (intern (concat "ghc-" (symbol-name n) "-key"))) vs)) )

(defun ghc-mod-depricated-key-values ()
  (mapcar (lambda (s) (symbol-value s)) ghc-mod-depricated-key-vars) )

(defvar ghc-mod-initial-key-values (ghc-mod-depricated-key-values) )

;; TODO: Check if user tried to change keybindings using the depricated key-vars
;; and present a big warning message.

;; (equal
;;  (ghc-mod-depricated-key-values)
;;  ghc-mod-initial-key-values )

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

(defun ghc-mod-init ()
  (ghc-abbrev-init)
  (ghc-type-init)
  (ghc-comp-init)
  (ghc-check-syntax)
  (defadvice save-buffer (after ghc-check-syntax-on-save activate)
    "Check syntax with GHC when a haskell-mode buffer is saved."
    (when (eq 'haskell-mode major-mode) (ghc-check-syntax))) )

(defun ghc-mod-deinit ()
  (ghc-abbrev-deinit)
  (ghc-type-deinit)
  (ghc-comp-deinit) )

(defvar ghc-mod-default-lighter " GhcMod")
(defvar-local ghc-mod-lighter ghc-mod-default-lighter)

;; TODO: globalized::
;;     (add-hook 'haskell-mode-hook 'ghc-mod-mode-init)
;;     (remove-hook 'haskell-mode-hook 'ghc-mod-mode-init)

(defun ghc-abbrev-init ()
  (set (make-local-variable 'dabbrev-case-fold-search) nil))

(defun ghc-abbrev-deinit ()
  (kill-local-variable 'dabbrev-case-fold-search) )

;;;###autoload
(define-minor-mode ghc-mod-mode
  nil ;; TODO: docs
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
