;;; init.el --- Clean, fast, modern config  -*- lexical-binding: t; -*-
;; -------------------------------------------------------------------
;; Startup performance
;; -------------------------------------------------------------------

;; instead of C-x-o, SHIFT+arrows
(windmove-default-keybindings)

;; remove the first page
(setq inhibit-startup-screen t)

;; number in column
(global-display-line-numbers-mode 1)
(column-number-mode 1)

;; yes->y no->n (replace)
(fset 'yes-or-no-p 'y-or-n-p)

;; automatic load file after changing (by git,..)
(global-auto-revert-mode 1)

;;Show Matching Parentheses
(show-paren-mode 1)

;; improve C-s to search
(use-package swiper
  :bind (("C-s" . swiper)))

;;Highlight Matching Brackets
(setq show-paren-delay 0)
(set-face-attribute 'show-paren-match nil :background "#44475a" :weight 'bold)

;; Save Your Place in Files (close emacs in line 34 when i return i return on 34)
(save-place-mode 1)

;; remove unnacessary buffer like message,..
(defun kill-unwanted-buffers ()
  "Kill annoying built-in buffers not created by user."
  (interactive)
  (dolist (buf '("*Messages*" "*scratch*" "*Completions*" "*Warnings*" "*Help*"))
    (when (get-buffer buf)
      (kill-buffer buf))))

;; Run once on startup
(add-hook 'emacs-startup-hook #'kill-unwanted-buffers)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 128 1024 1024)
                  gc-cons-percentage 0.1)))

(let ((old-file-name-handler-alist file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda () (setq file-name-handler-alist old-file-name-handler-alist))))

(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors nil))

(setq inhibit-startup-message t
      ring-bell-function 'ignore
      use-dialog-box nil
      frame-resize-pixelwise t)

;; -------------------------------------------------------------------
;; Theme & font (keeps your choices)
;; -------------------------------------------------------------------
(load-theme 'tango-dark t)
(when (member "JetBrains Mono" (font-family-list))
  (set-face-attribute 'default nil :font "JetBrains Mono-11"))

;; -------------------------------------------------------------------
;; Package Management (use MELPA/GNU/NonGNU + bootstrap use-package)
;; -------------------------------------------------------------------
(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))
(unless (bound-and-true-p package--initialized)
  (package-initialize))
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t
      use-package-expand-minimally t)

;; Proactively install core packages; ignore failures (e.g., offline); we fall back gracefully if missing.
(let ((core-pkgs '(orderless vertico marginalia consult corfu cape no-littering
                             flycheck flycheck-eglot eglot format-all
                             yaml-mode json-mode toml-mode magit doom-modeline which-key)))
  (unless package-archive-contents (ignore-errors (package-refresh-contents)))
  (dolist (p core-pkgs)
    (unless (package-installed-p p)
      (ignore-errors (package-install p)))))

;; -------------------------------------------------------------------
;; UI / Usability
;; -------------------------------------------------------------------
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom (doom-modeline-height 15))

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom (which-key-idle-delay 0.5))


(save-place-mode 1)
(recentf-mode 1)
(savehist-mode 1)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(electric-pair-mode 1)

;; Better defaults
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 100)
(setq sentence-end-double-space nil)

;; Keep user config separate
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;; Enable standard keybindings (C-c/C-v etc.)
(cua-mode 1)
(setq cua-keep-region-after-copy t)

;; -------------------------------------------------------------------
;; Tidy filesystem: put backups/autosaves under ~/.emacs.d/var
;; -------------------------------------------------------------------
(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory)
        no-littering-var-directory (expand-file-name "var/" user-emacs-directory))
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        backup-directory-alist
        `(("." . ,(no-littering-expand-var-file-name "backup/")))
        make-backup-files t
        delete-old-versions t
        kept-new-versions 10
        kept-old-versions 2
        version-control t))

;; -------------------------------------------------------------------
;; Completion & search (robust bootstrapping + graceful fallback)
;; -------------------------------------------------------------------
(condition-case err
    (progn
      (use-package vertico :init (vertico-mode 1))
      (use-package orderless :defer t)
      (use-package marginalia :init (marginalia-mode 1))
      (use-package consult
        :bind (("C-s"   . consult-line)
               ("C-x b" . consult-buffer)
               ("M-g g" . consult-goto-line)
               ("C-c r" . consult-ripgrep)))
      (use-package corfu
        :init (global-corfu-mode)
        :custom (corfu-auto t))
      (use-package cape
        :init (add-to-list 'completion-at-point-functions #'cape-file))
      ;; If orderless is present, enable it; else fall back to basic completion.
      (if (require 'orderless nil 'noerror)
          (progn
            (setq completion-styles '(orderless basic)
                  completion-category-defaults nil
                  completion-category-overrides '((file (styles basic partial-completion)))))
        (message "orderless not available; falling back to basic completion.")
        (setq completion-styles '(basic partial-completion)
              completion-category-defaults nil
              completion-category-overrides '((file (styles basic partial-completion))))))
  (error
   (message "Completion stack setup failed: %s. Using defaults." err)))

;; -------------------------------------------------------------------
;; Editing & Formatting
;; -------------------------------------------------------------------
(use-package format-all
  :hook ((prog-mode . format-all-mode)
         (format-all-mode . format-all-ensure-formatter))
  :bind (("C-c f" . format-all-buffer)))

;; Don't shadow the default undo on C-/
(global-set-key (kbd "C-c /") #'comment-line)

;; -------------------------------------------------------------------
;; Syntax Checking + LSP integration
;; -------------------------------------------------------------------
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom (flycheck-display-errors-delay 0.25))

;; Prefer Eglot (built-in in Emacs 29) for LSP diagnostics, bridged into Flycheck UI
(use-package eglot
  :hook ((go-mode
          python-mode
          js-mode
          typescript-ts-mode
          json-mode
          yaml-mode
          rust-mode
          c-mode c++-mode
          java-mode) . eglot-ensure)
  :config
  ;; Java via jdtls if available
  (when (executable-find "jdtls")
    (add-to-list 'eglot-server-programs '(java-mode . ("jdtls")))))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :config (global-flycheck-eglot-mode 1))

;; -------------------------------------------------------------------
;; Languages & tools
;; -------------------------------------------------------------------
(use-package markdown-mode :mode ("\\.md\\'" "\\.markdown\\'")
  :custom (markdown-command "pandoc"))
(use-package yaml-mode :mode "\\.ya?ml\\'")
(use-package json-mode :mode "\\.json\\'")
(use-package toml-mode :mode "\\.toml\\'")
(use-package magit :commands (magit-status))

;; Simple project keybindings (built-in project.el)
(use-package project :ensure nil)
(global-set-key (kbd "C-c p f") #'project-find-file)
(global-set-key (kbd "C-c p s") #'project-switch-project)

;;; init.el ends here

