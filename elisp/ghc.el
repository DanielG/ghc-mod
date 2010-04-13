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

(defvar ghc-version "0.2.0")

;; (eval-when-compile
;;  (require 'haskell-mode))

(require 'ghc-comp)
(require 'ghc-doc)
(require 'ghc-flymake)
(require 'ghc-command)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initializer
;;;

(defvar ghc-initialized nil)

(defun ghc-init ()
  (unless ghc-initialized
    (define-key haskell-mode-map ghc-completion-key  'ghc-complete)
    (define-key haskell-mode-map ghc-document-key    'ghc-browse-document)
    (define-key haskell-mode-map ghc-import-key      'ghc-load-module-buffer)
    (define-key haskell-mode-map ghc-previous-key    'flymake-goto-prev-error)
    (define-key haskell-mode-map ghc-next-key        'flymake-goto-next-error)
    (define-key haskell-mode-map ghc-help-key        'ghc-display-errors)
    (define-key haskell-mode-map ghc-insert-key      'ghc-insert-template)
    (define-key haskell-mode-map ghc-sort-key        'ghc-sort-lines)
    (ghc-comp-init)
    (setq ghc-initialized t)))

(provide 'ghc)