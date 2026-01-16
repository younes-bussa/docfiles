;;; early-init.el -*- lexical-binding: t; -*-

;; Don’t let Emacs auto-init packages before init.el
(setq package-enable-at-startup nil)

;; Startup GC boost
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

;; Idle GC
(when (fboundp 'garbage-collect)
  (ignore-errors (cancel-function-timers #'garbage-collect))
  (run-with-idle-timer 5 t #'garbage-collect))

;; Silence native-comp spam
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors nil))

;; Kill UI chrome ASAP
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t
      inhibit-startup-message t
      use-dialog-box nil
      frame-resize-pixelwise t)

