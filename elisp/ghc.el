;;; ghc.el --- ghc-mod front-end for haskell-mode

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 25, 2009
;; Revised:

;; Put the following code to your "~/.emacs".
;;
;; (autoload 'ghc-init "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;;
;; Or if you wish to display error each goto next/prev error,
;; set ghc-display-error valiable.
;;
;; (setq ghc-display-error 'minibuffer) ; to minibuffer
;; ; (setq ghc-display-error 'other-buffer) ; to other-buffer

;;

;;; Code:

(defconst ghc-version "4.0.2")

;; (eval-when-compile
;;  (require 'haskell-mode))

(require 'ghc-comp)
(require 'ghc-doc)
(require 'ghc-info)
(require 'ghc-check)
(require 'ghc-command)
(require 'ghc-ins-mod)
(require 'ghc-indent)
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
(defvar ghc-check-key       "\C-x\C-s")
(defvar ghc-toggle-key      "\C-c\C-c")
(defvar ghc-jump-key        "\C-c\C-j")
(defvar ghc-module-key      "\C-c\C-m")
(defvar ghc-expand-key      "\C-c\C-e")
(defvar ghc-kill-key        "\C-c\C-k")
(defvar ghc-hoogle-key      (format "\C-c%c" (ghc-find-C-h)))
(defvar ghc-shallower-key   "\C-c<")
(defvar ghc-deeper-key      "\C-c>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initializer
;;;

(defvar ghc-initialized nil)

;;;###autoload
(defun ghc-init ()
  (ghc-abbrev-init)
  (ghc-type-init)
  (unless ghc-initialized
    (define-key haskell-mode-map ghc-completion-key  'ghc-complete)
    (define-key haskell-mode-map ghc-document-key    'ghc-browse-document)
    (define-key haskell-mode-map ghc-type-key        'ghc-show-type)
    (define-key haskell-mode-map ghc-info-key        'ghc-show-info)
    (define-key haskell-mode-map ghc-expand-key      'ghc-expand-th)
    (define-key haskell-mode-map ghc-import-key      'ghc-import-module)
    (define-key haskell-mode-map ghc-previous-key    'ghc-goto-prev-error)
    (define-key haskell-mode-map ghc-next-key        'ghc-goto-next-error)
    (define-key haskell-mode-map ghc-help-key        'ghc-display-errors)
    (define-key haskell-mode-map ghc-insert-key      'ghc-insert-template)
    (define-key haskell-mode-map ghc-sort-key        'ghc-sort-lines)
    (define-key haskell-mode-map ghc-check-key       'ghc-save-buffer)
    (define-key haskell-mode-map ghc-toggle-key      'ghc-toggle-check-command)
    (define-key haskell-mode-map ghc-jump-key        'ghc-jump-file)
    (define-key haskell-mode-map ghc-module-key      'ghc-insert-module)
    (define-key haskell-mode-map ghc-kill-key        'ghc-kill-process)
    (define-key haskell-mode-map ghc-hoogle-key      'haskell-hoogle)
    (define-key haskell-mode-map ghc-shallower-key   'ghc-make-indent-shallower)
    (define-key haskell-mode-map ghc-deeper-key      'ghc-make-indent-deeper)
    (ghc-comp-init)
    (setq ghc-initialized t))
  (ghc-check-syntax))

(defun ghc-abbrev-init ()
  (set (make-local-variable 'dabbrev-case-fold-search) nil))

(provide 'ghc)
