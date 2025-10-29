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
(run-with-idle-timer 2 nil #'garbage-collect)

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

(dolist (pkg '(use-package flycheck no-littering vertico marginalia consult corfu cape rainbow-delimiters))
  (unless (package-installed-p pkg) (package-install pkg)))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t
      use-package-expand-minimally t)

;;; ============================== UX basics ==================================
;; Movement, clipboard, status
(windmove-default-keybindings 'meta)  ; M-←/→/↑/↓
(setq windmove-wrap-around t)
(cua-mode 1)
(setq cua-keep-region-after-copy t)
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Parens & pairing
(setq show-paren-delay 0 show-paren-style 'mixed)
(show-paren-mode 1)
(electric-pair-mode 1)

;; History & auto-revert
(save-place-mode 1)
(recentf-mode 1)
(savehist-mode 1)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; Editing defaults
(setq-default indent-tabs-mode nil tab-width 4 fill-column 100)
(setq sentence-end-double-space nil)

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
        make-backup-files t delete-old-versions t kept-new-versions 10 kept-old-versions 2
        version-control t))

;;; ============================ Completion/search ============================
(use-package vertico :init (vertico-mode 1))
(use-package marginalia :init (marginalia-mode 1))
(use-package consult
  :bind (("C-s"   . consult-line)
         ("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("C-c r" . consult-ripgrep))) ; requires ripgrep

(use-package corfu :init (global-corfu-mode) :custom (corfu-auto t))
(use-package cape  :init (add-to-list 'completion-at-point-functions #'cape-file))

;; Prefer orderless if present; fall back cleanly for files
(when (require 'orderless nil 'noerror)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))
(unless (featurep 'orderless)
  (setq completion-styles '(basic partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;;; ============================== Theme & fonts ==============================
(load-theme 'tango-dark t)

(when (and (display-graphic-p) (member "JetBrains Mono" (font-family-list)))
  (set-face-attribute 'default nil :family "JetBrains Mono" :height 115))
(set-face-attribute 'show-paren-match nil :weight 'bold)

;; Minimal chrome & scrolling
(when (fboundp 'tool-bar-mode)              (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)            (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(setq-default cursor-type 'bar)
(when (fboundp 'pixel-scroll-precision-mode) (pixel-scroll-precision-mode 1))
(setq scroll-margin 5 scroll-step 1 scroll-conservatively 10000
      scroll-preserve-screen-position t mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil)

;;; ========== Current-line highlight: on in code/special, off while writing ===

;; Disable global line highlight
(global-hl-line-mode -1)

;; Enable only for code and special buffers
(add-hook 'prog-mode-hook    #'hl-line-mode)
(add-hook 'special-mode-hook #'hl-line-mode)

;; Explicitly disable in writing modes
(add-hook 'text-mode-hook    (lambda () (hl-line-mode -1)))
(add-hook 'org-mode-hook     (lambda () (hl-line-mode -1)))
(add-hook 'markdown-mode-hook (lambda () (hl-line-mode -1)))

;; Make it clearly visible on dark themes (like Wombat)
(with-eval-after-load 'hl-line
  (set-face-attribute 'hl-line nil
                      :background "#3c3f41"  ;; dark gray highlight
                      :underline nil
                      :inherit nil))


;;; ====================== Programmer helpers / visuals =======================
(setq font-lock-maximum-decoration t)
;;(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))


