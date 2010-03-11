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

(defvar ghc-version "0.2.0")

;; (eval-when-compile
;;  (require 'haskell-mode))

(require 'ghc-comp)
(require 'ghc-doc)
(require 'ghc-flymake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customize Variables
;;;

(defvar ghc-completion-key "\e\t")
(defvar ghc-document-key   "\e\C-d")
(defvar ghc-import-key     "\e\C-m")
(defvar ghc-check-key      "\e\C-c")
(defvar ghc-previous-key   "\e\C-p")
(defvar ghc-next-key       "\e\C-n")
(defvar ghc-help-key       "\e?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initializer
;;;

(defvar ghc-initialized nil)

(defun ghc-init ()
  (unless ghc-initialized
    (define-key haskell-mode-map ghc-completion-key 'ghc-complete)
    (define-key haskell-mode-map ghc-document-key   'ghc-browse-document)
    (define-key haskell-mode-map ghc-import-key     'ghc-load-module-buffer)
    (define-key haskell-mode-map ghc-check-key      'flymake-start-syntax-check)
    (define-key haskell-mode-map ghc-previous-key   'flymake-goto-prev-error)
    (define-key haskell-mode-map ghc-next-key       'flymake-goto-next-error)
    (define-key haskell-mode-map ghc-help-key       'flymake-display-err-menu-for-current-line)
    (ghc-comp-init)
    (setq ghc-initialized t)))

(provide 'ghc)