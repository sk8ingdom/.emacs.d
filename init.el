;; -*- mode: elisp -*-

(cond
 ;; Windows
 ((string-equal system-type "windows-nt")
  (load "~/.emacs.d/windows-config/windows")
  (load "~/.emacs.d/org-mode-config/windows-org")
 )
 ;; OSX
 ((string-equal system-type "darwin")
 )
 ;; Linux
 ((string-equal system-type "gnu/linux")
  (load "~/.emacs.d/linux-config/linux")
  (load "~/.emacs.d/org-mode-config/linux-org")
 )
)
 ;; General
(load "~/.emacs.d/general-config/general")

;; Org-mode
(load "~/.emacs.d/org-mode-config/general-org")
(load "~/.emacs.d/org-mode-config/to-do-states")
(load "~/.emacs.d/org-mode-config/capture-templates")

;; Add-ins
;; (load "~/.emacs.d/add-ins/calcfw-org")
(load "~/.emacs.d/add-ins/htmlize")
(load "~/.emacs.d/add-ins/ob-calc")
;; No longer require the actual org-protocol file as it's included by default in 24.2
;; (load "~/.emacs.d/add-ins/org-protocol")
(require 'org-protocol)
(server-start)
