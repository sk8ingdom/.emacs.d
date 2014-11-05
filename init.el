;; -*- mode: elisp -*-

 ;; General
(load "~/.emacs.d/general-config/general")

;; Org-mode
(load "~/.emacs.d/org-mode-config/org-general")
(load "~/.emacs.d/org-mode-config/org-todo-states")
(load "~/.emacs.d/org-mode-config/org-capture-templates")
(load "~/.emacs.d/org-mode-config/org-agenda-custom")

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
