;;; my-core-settings.el -*- lexical-binding: t; -*-

;; Simple prompts
(defalias 'yes-or-no-p #'y-or-n-p)

;; Column numbers
(column-number-mode 1)

;; Auto-revert files
(setq auto-revert-verbose nil)
(global-auto-revert-mode 1)

;; Remember cursor position
(setq save-place-file (locate-user-emacs-file "places"))
(save-place-mode 1)

;; GC tuning during minibuffer usage
(defun my/minibuffer-setup-gc ()
  (setq gc-cons-threshold (* 512 1024 1024)))
(defun my/minibuffer-exit-gc ()
  (setq gc-cons-threshold (* 64 1024 1024)))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-gc)
(add-hook 'minibuffer-exit-hook  #'my/minibuffer-exit-gc)

(provide 'my-core-settings)
