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

;; Automatically convert line endings to unix
;;(add-hook 'find-file-hook 'find-file-check-line-endings)
;;(defun dos-file-endings-p ()
;;	(string-match "dos" (symbol-name buffer-file-coding-system)))
;;(defun find-file-check-line-endings ()
;;	(when (dos-file-endings-p)
;;		(set-buffer-file-coding-system 'undecided-unix)
;;		(set-buffer-modified-p nil)))

;; Enable follow link bindings
(ffap-bindings)

;; Plugins

;; Package
(require 'package)
(package-initialize)

;; Enable nastran-mode
;; (load "~/.emacs.d/plugins/nastran-mode")

;; Enable htmlize
(load "~/.emacs.d/plugins/htmlize")

;; Enable request
(load "~/.emacs.d/plugins/request")

;; Enable web-mode
;; (load "~/.emacs.d/plugins/web-mode")

;; Start Emacs server
(server-start)
