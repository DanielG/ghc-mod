;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ghc.el
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 25, 2009
;; Revised:

;; Put the following code to your "~/.emacs".
;;
;; (autoload 'ghc-init "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;; Or
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

;;; Code:

(defconst ghc-version "0.5.3")

;; (eval-when-compile
;;  (require 'haskell-mode))

(require 'ghc-comp)
(require 'ghc-doc)
(require 'ghc-info)
(require 'ghc-flymake)
(require 'ghc-command)
(require 'dabbrev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customize Variables
;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initializer
;;;

(defvar ghc-initialized nil)

(defun ghc-init ()
  (ghc-abbrev-init)
  (unless ghc-initialized
    (define-key haskell-mode-map ghc-completion-key  'ghc-complete)
    (define-key haskell-mode-map ghc-document-key    'ghc-browse-document)
    (define-key haskell-mode-map ghc-type-key        'ghc-show-type)
    (define-key haskell-mode-map ghc-info-key        'ghc-show-info)
    (define-key haskell-mode-map ghc-import-key      'ghc-import-module)
    (define-key haskell-mode-map ghc-previous-key    'flymake-goto-prev-error)
    (define-key haskell-mode-map ghc-next-key        'flymake-goto-next-error)
    (define-key haskell-mode-map ghc-help-key        'ghc-flymake-display-errors)
    (define-key haskell-mode-map ghc-insert-key      'ghc-insert-template)
    (define-key haskell-mode-map ghc-sort-key        'ghc-sort-lines)
    (define-key haskell-mode-map ghc-check-key       'ghc-save-buffer)
    (define-key haskell-mode-map ghc-toggle-key      'ghc-flymake-toggle-command)
    (ghc-comp-init)
    (setq ghc-initialized t)))

(defun ghc-abbrev-init ()
  (set (make-local-variable 'dabbrev-case-fold-search) nil))

(provide 'ghc)
