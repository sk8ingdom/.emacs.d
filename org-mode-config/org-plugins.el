;; Plugins

;; Enable ob-calc
(require 'ob-calc)

;; Enable org-mobile
(require 'org-mobile)

;; Enable org-collector
(require 'org-collector)

;; Enable org-protocol
(require 'org-protocol)
(load "~/.emacs.d/org-mode-config/org-protocol-templates")

;; Enable org-contacts
(require 'org-contacts)
(add-to-list 'org-contacts-files (concat org-directory "/peo.org"))
(setq org-contacts-icon-use-gravatar nil)
