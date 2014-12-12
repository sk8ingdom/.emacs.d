;; Plugins

;; Enable calcfw-org
;; (load "~/.emacs.d/plugins/calcfw-org")

;; Enable ob-calc
(load "~/.emacs.d/plugins/ob-calc")

;; Enable org-mobile
(load "~/.emacs.d/plugins/org-mobile")

;; Enable org-collector
(load "~/.emacs.d/plugins/org-collector")

;; Enable org-protocol
(require 'org-protocol)
(load "~/.emacs.d/org-mode-config/org-protocol-templates")

;; Enable org-contacts
;; No need to set org-contacts-files, defaults to agenda files
(load "~/.emacs.d/plugins/org-contacts")
(add-to-list 'org-contacts-files (concat org-directory "/peo.org"))
