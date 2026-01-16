;;; my-completion.el -*- lexical-binding: t; -*-

(use-package savehist
  :init (savehist-mode 1))

(use-package vertico
  :init
  (vertico-mode 1)
  :config
  (setq vertico-cycle t))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles basic partial-completion)))))

(use-package consult
  :bind (("C-s"   . consult-line)
         ("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("C-c r" . consult-ripgrep)
         ("M-i"   . consult-imenu)))

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.20)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  (corfu-preselect 'prompt))

(use-package cape
  :after corfu
  :init
  (add-hook
   'prog-mode-hook
   (lambda ()
     (add-to-list 'completion-at-point-functions #'cape-dabbrev)
     (add-to-list 'completion-at-point-functions #'cape-file))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'my-completion)
