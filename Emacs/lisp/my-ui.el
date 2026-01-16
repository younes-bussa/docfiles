;;; my-ui.el -*- lexical-binding: t; -*-

;; Theme only for GUI
(when (display-graphic-p)
  (load-theme 'tango-dark t))

;; Parens, highlight, etc.
(setq show-paren-delay 0)
(show-paren-mode 1)

(electric-pair-mode 1)
(setq font-lock-maximum-decoration t)

;; Line numbers in code
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(provide 'my-ui)
