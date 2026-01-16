;;; init.el -*- lexical-binding: t; -*-

;; 1. Keep custom.el out of the way
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; 2. Add ~/.emacs.d/lisp to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; 3. Bootstrap package system + use-package (one place only)
(require 'my-core-packages)

;; 4. Core behavior
(require 'my-core-settings)   ; GC around minibuffer, basic defaults
(require 'my-ui)              ; theme, modeline, visuals
(require 'my-editing)         ; tabs, parens, movement, backups
(require 'my-completion)      ; vertico, corfu, orderless, consult, etc.
(require 'my-lsp)             ; eglot + flycheck config
(require 'my-misc)            ; which-key, magit, etc.
