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

;;; ============================== UX basics ==================================
;; Movement, clipboard, status
(windmove-default-keybindings 'meta)  ; M-←/→/↑/↓
(setq windmove-wrap-around t)
(cua-mode 1)
(setq cua-keep-region-after-copy t)
(column-number-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Line numbers: only where it’s cheap/useful
(defun my/enable-line-numbers ()
  (when (and (derived-mode-p 'prog-mode 'conf-mode 'text-mode)
             (< (buffer-size) 500000))      ; ~500KB guard
    (display-line-numbers-mode 1)))
(global-display-line-numbers-mode -1)
(add-hook 'prog-mode-hook #'my/enable-line-numbers)
(add-hook 'text-mode-hook #'my/enable-line-numbers)

;; Parens & pairing
(setq show-paren-delay 0
      show-paren-style 'mixed)
(show-paren-mode 1)
(electric-pair-mode 1)

;; History & auto-revert
(save-place-mode 1)
(recentf-mode 1)
(savehist-mode 1)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; Editing defaults
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 100)
(setq sentence-end-double-space nil)

;; Big-file safety (built-in)
(when (boundp 'so-long-mode) (global-so-long-mode 1))

;;; ============================ Filesystem hygiene ============================
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

;; recentf/TRAMP hardening to avoid I/O stalls
(with-eval-after-load 'recentf
  (setq recentf-max-saved-items 300
        recentf-auto-cleanup 'never
        recentf-exclude '("/tmp/" "/ssh:" "/sudo:" "\\.gz\\'" "\\.zip\\'" "\\.eln\\'")))
(setq tramp-auto-save-directory (expand-file-name "tramp-auto-save/" temporary-file-directory)
      tramp-default-method "ssh")

;;; ============================ Completion/search ============================
(use-package vertico :init (vertico-mode 1))
(use-package marginalia :init (marginalia-mode 1))

(use-package consult
  :bind (("C-s"   . consult-line)
         ("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("C-c r" . consult-ripgrep))) ; requires ripgrep installed

;; Prefer orderless; fall back cleanly for files
(when (require 'orderless nil 'noerror)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))
(unless (featurep 'orderless)
  (setq completion-styles '(basic partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.20)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  (corfu-preselect 'prompt))

;; CAPE: scope file completion to code-ish buffers to reduce disk hits
(use-package cape
  :init
  (defun my/cape-setup ()
    (add-to-list 'completion-at-point-functions #'cape-file))
  (add-hook 'prog-mode-hook #'my/cape-setup))

;;; ============================== Theme & fonts ==============================
(load-theme 'tango-dark t)

;; Avoid full font enumeration; use find-font for O(1) lookup
(when (and (display-graphic-p)
           (find-font (font-spec :name "JetBrains Mono")))
  (set-face-attribute 'default nil :family "JetBrains Mono" :height 115))
(set-face-attribute 'show-paren-match nil :weight 'bold)

;; Minimal chrome & scrolling
(when (fboundp 'tool-bar-mode)              (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)            (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(setq-default cursor-type 'bar)

;; Scrolling performance knobs
(setq fast-but-imprecise-scrolling t
      jit-lock-defer-time 0.05
      scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode -1))  ; disable expensive precision mode

;;; ========== Current-line highlight: on in code/special, off while writing ===
;; Disable global line highlight
(global-hl-line-mode -1)
;; Enable only for code and special buffers
(add-hook 'prog-mode-hook    #'hl-line-mode)
(add-hook 'special-mode-hook #'hl-line-mode)
;; Explicitly disable in writing modes
(add-hook 'text-mode-hook      (lambda () (hl-line-mode -1)))
(add-hook 'org-mode-hook       (lambda () (hl-line-mode -1)))
(add-hook 'markdown-mode-hook  (lambda () (hl-line-mode -1)))
;; Make it clearly visible on dark themes
(with-eval-after-load 'hl-line
  (set-face-attribute 'hl-line nil
                      :background "#3c3f41"  ;; dark gray highlight
                      :underline nil
                      :inherit nil))

;;; ====================== Programmer helpers / visuals =======================
(setq font-lock-maximum-decoration t)
;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

;;; =============================== GC guards =================================
;; Raise GC threshold while minibuffer is active (completions, searches)
(defun my/minibuffer-setup-gc ()
  (setq gc-cons-threshold (* 512 1024 1024)))
(defun my/minibuffer-exit-gc ()
  (setq gc-cons-threshold (* 64 1024 1024)))
(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-gc)
(add-hook 'minibuffer-exit-hook  #'my/minibuffer-exit-gc)
