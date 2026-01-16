;;; my-lsp.el -*- lexical-binding: t; -*-

(use-package eglot
  :hook ((c-mode c++-mode python-mode js-mode) . eglot-ensure))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "f") #'project-find-file)
  (define-key project-prefix-map (kbd "g") #'project-find-regexp)
  (define-key project-prefix-map (kbd "b") #'project-switch-to-buffer))

(provide 'my-lsp)
