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

;;; Code:

(defvar ghc-version "0.1")

;; (require 'haskell-mode)
(require 'ghc-comp)
(require 'ghc-doc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customize Variables
;;;

(defvar ghc-completion-key "\e\t")
(defvar ghc-document-key   "\e\C-d")
(defvar ghc-import-key     "\e\C-e")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initializer
;;;

(defvar ghc-initialized nil)

(defun ghc-init ()
  (unless ghc-initialized
    (define-key haskell-mode-map ghc-completion-key 'ghc-complete)
    (define-key haskell-mode-map ghc-document-key   'ghc-browse-document)
    (define-key haskell-mode-map ghc-import-key     'ghc-import-module)
    (ghc-comp-init)
    (setq ghc-initialized t)))



