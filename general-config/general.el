;; Package management
(require 'package)
(package-initialize)

;; Disable the splash screen
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode t)

;; Calc mode
;; Disable multiplication having precedence over division
(setq calc-multiplication-has-precedence nil)

;; Disable git
(setq vc-handled-backends ())

;; Disable backup
(setq backup-inhibited t)

;; Disable auto-save
(setq auto-save-default nil)

(server-start)
