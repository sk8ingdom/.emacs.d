;; -*- mode: elisp -*-

 ;; General
(load "~/.emacs.d/general-config/general")

;; Org-mode
(load "~/.emacs.d/org-mode-config/general-org")
(load "~/.emacs.d/org-mode-config/to-do-states")
(load "~/.emacs.d/org-mode-config/capture-templates")
(load "~/.emacs.d/org-mode-config/agenda-custom")

(cond
 ;; Windows
 ((string-equal system-type "windows-nt")
  (load "~/.emacs.d/windows-config/windows")
  (load "~/.emacs.d/org-mode-config/windows-org")
 )
 ;; OSX
 ((string-equal system-type "darwin")
  (load "~/.emacs.d/osx-config/osx")
  (load "~/.emacs.d/org-mode-config/osx-org")
 )
 ;; Linux
 ((string-equal system-type "gnu/linux")
  (load "~/.emacs.d/linux-config/linux")
  (load "~/.emacs.d/org-mode-config/linux-org")
 )
)
