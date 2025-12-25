;; -*- lexical-binding: t; -*-

;;; =========================== Quiet, fast startup ===========================
(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      use-dialog-box nil
      frame-resize-pixelwise t)

;; GC + file-name-handler boost (robust: no void variables later)
(defvar my/old-gc-cons-threshold gc-cons-threshold)
(defvar my/old-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold (* 256 1024 1024)
      gc-cons-percentage 0.6
      file-name-handler-alist nil)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist my/old-file-name-handler-alist))
          100)

;; Idle GC: run periodically when Emacs is idle (smoother than a one-shot)
(when (fboundp 'garbage-collect)
  (ignore-errors (cancel-function-timers #'garbage-collect))
  (run-with-idle-timer 5 t #'garbage-collect))

;; Silence native-comp warnings (Emacs 28+)
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors nil))

;;; ============================== Packages ===================================
(setq package-enable-at-startup nil)
(require 'package)
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities '(("gnu" . 100) ("nongnu" . 80) ("melpa-stable" . 60)))
(unless (bound-and-true-p package--initialized) (package-initialize))
(unless package-archive-contents (package-refresh-contents))

;; Ensure core packages are present
(dolist (pkg '(use-package flycheck no-littering vertico marginalia consult corfu cape
                rainbow-delimiters orderless))
  (unless (package-installed-p pkg) (package-install pkg)))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t
      use-package-expand-minimally t)

;; themes for GUI
(when (display-graphic-p)
  (load-theme 'tango-dark t))

;; auto parenthese
(setq show-paren-delay 0)
(show-paren-mode 1)
(electric-pair-mode 1)         ;; auto-close (), {}, [], "", ''

;; Defer packages by default
(setq use-package-always-defer t)

;; remember line
(setq save-place-file (locate-user-emacs-file "places"))
(save-place-mode 1)

;; jump symbol function,..
(global-set-key (kbd "M-i") #'consult-imenu)

;; clean interface
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;; packages handling manually by
;; M-x package-refresh-contents
;; (unless package-archive-contents
;;   (package-refresh-contents))
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))
;; (require 'use-package)
;; (setq use-package-always-ensure t)

;; line number
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; M-←/→/↑/↓ move between windows
(windmove-default-keybindings 'meta) 
(setq windmove-wrap-around t)
;; Enables CUA mode C-c copy,...
(cua-mode 1)
(setq cua-keep-region-after-copy t)
(column-number-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Stops Emacs from spamming messages when buffers auto-refresh. Keeps the echo area clean.
(setq auto-revert-verbose nil)
(global-auto-revert-mode 1)

;; tab 4 space 
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Filesystem hygiene
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

;; vertico
(use-package vertico
  :init
  (vertico-mode 1)
  :config
  (setq vertico-cycle t)) ;; wrap at list end

(use-package marginalia :init (marginalia-mode 1))

(use-package consult
  :bind (
         ("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("C-c r" . consult-ripgrep))
  ) ; requires ripgrep installed

(use-package savehist
  :init (savehist-mode 1))

;; Prefer orderless; fall back cleanly for files
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles basic partial-completion)))))

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.20)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  (corfu-preselect 'prompt))

;; maximum syntax highlighting
(setq font-lock-maximum-decoration t)

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Project management
(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "f") #'project-find-file)
  (define-key project-prefix-map (kbd "g") #'project-find-regexp)
  (define-key project-prefix-map (kbd "b") #'project-switch-to-buffer))

;; LSP (Eglot) + diagnostics (Flycheck or Flymake)
(use-package eglot
  :hook ((c-mode c++-mode python-mode js-mode) . eglot-ensure))

;; which-key
(use-package which-key
  :init (which-key-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(avy breadcrumb cape consult corfu doom-modeline
         eglot-inactive-regions exec-path-from-shell expand-region
         flycheck-eglot format-all gnu-elpa-keyring-update json-mode
         magit marginalia markdown-preview-mode mixed-pitch
         modus-themes no-littering orderless php-mode python-black
         rainbow-delimiters swiper symbol-overlay toml-mode undo-tree
         vertico yaml-mode yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; =============================== GC guards =================================
;; Raise GC threshold while minibuffer is active (completions, searches)
(defun my/minibuffer-setup-gc ()
  (setq gc-cons-threshold (* 512 1024 1024)))
(defun my/minibuffer-exit-gc ()
  (setq gc-cons-threshold (* 64 1024 1024)))
(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-gc)
(add-hook 'minibuffer-exit-hook  #'my/minibuffer-exit-gc)
