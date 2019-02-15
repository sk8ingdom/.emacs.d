;; -*- mode: emacs-lisp -*-

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.emacs.d/general-config/general")
(load "~/.emacs.d/general-config/general-plugins")
(load "~/.emacs.d/general-config/general-modes")

;; Org-mode
(load "~/.emacs.d/org-mode-config/org-general")
(load "~/.emacs.d/org-mode-config/org-todo-states")
(load "~/.emacs.d/org-mode-config/org-capture-templates")
(load "~/.emacs.d/org-mode-config/org-agenda-custom")
(load "~/.emacs.d/org-mode-config/org-abbreviations")
(load "~/.emacs.d/org-mode-config/org-hyperlinks")

(cond
 ;; Windows
 ((string-equal system-type "windows-nt")
  (load "~/.emacs.d/windows-config/windows")
  (load "~/.emacs.d/org-mode-config/org-windows")
  )
 ;; OSX
 ((string-equal system-type "darwin")
  (load "~/.emacs.d/osx-config/osx")
  (load "~/.emacs.d/org-mode-config/org-osx")
  )
 ;; Linux
 ((string-equal system-type "gnu/linux")
  (load "~/.emacs.d/linux-config/linux")
  (load "~/.emacs.d/org-mode-config/org-linux")
  )
 )

;; Gnus
(setq gnus-init-file "~/.emacs.d/.gnus.el")

;; Org-mode Plugins
(load "~/.emacs.d/org-mode-config/org-plugins")

;; Work Functions and variables
(load "~/.emacs.d/wrk/wrk.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-dictionary "american")
 '(ispell-program-name "C:/cygwin64/bin/aspell.exe")
 '(package-selected-packages
   (quote
    (fireplace csv-mode org-noter org-journal calfw-org calfw python-mode with-simulated-input which-key use-package undo-tree symon sx shackle org-plus-contrib org-pdfview mwe-log-commands minimap minibuffer-line magithub helm-youtube helm-w32-launcher helm-swoop helm-spotify-plus helm-spotify helm-org-rifle helm-google google-maps elmacro dired-quick-sort dedicated counsel-spotify counsel command-log-mode auto-package-update adaptive-wrap ace-window ace-link ace-isearch))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
