;;; my-editing.el -*- lexical-binding: t; -*-

;; Tabs: 4 spaces
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Window movement
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; CUA keys
(cua-mode 1)
(setq cua-keep-region-after-copy t)

;; Backups + autosave via no-littering
(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory)
        no-littering-var-directory (expand-file-name "var/" user-emacs-directory))
  :config
  (setq make-backup-files t
        backup-by-copying t
        version-control t
        delete-old-versions t
        kept-new-versions 10
        kept-old-versions 2
        backup-directory-alist
        `(("." . ,(no-littering-expand-var-file-name "backup/")))
        auto-save-default t
        auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(provide 'my-editing)
