;; -*- mode: emacs-lisp -*-

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(load "~/.emacs.d/general-config/general")
(load "~/.emacs.d/general-config/general-plugins")

;; Org-mode
(load "~/.emacs.d/org-mode-config/org-general")
(load "~/.emacs.d/org-mode-config/org-todo-states")
(load "~/.emacs.d/org-mode-config/org-capture-templates")
(load "~/.emacs.d/org-mode-config/org-agenda-custom")
(load "~/.emacs.d/org-mode-config/org-abbreviations")

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
