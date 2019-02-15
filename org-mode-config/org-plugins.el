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

;; enable org-protocol-capture-html
;; Uses pandoc to convert :%initial
(require 'org-protocol-capture-html)
(setq org-protocol-capture-html-pandoc-no-wrap-option "--wrap=none")

;; org-web-tools
(require 'org-web-tools)
(setq org-web-tools-pandoc-sleep-time 5.0)

;; Enable org-contacts
(require 'org-contacts)
(add-to-list 'org-contacts-files (concat org-directory "/peo.org"))
(setq org-contacts-icon-use-gravatar nil)

;; Enable org-eww
(require 'org-eww)
(add-hook 'eww-after-render-hook 'eww-readable)
;; (remove-hook 'eww-after-render-hook 'eww-readable)
(add-hook 'eww-mode-hook
          (lambda ()
            (define-key eww-mode-map (kbd "o") #'org-eww-copy-for-org-mode)))

;; Enable org-cal
(setq package-check-signature nil)

(use-package org-gcal
  :ensure t
  :config
  (setq org-gcal-client-id "207160141408-gl9ejjpi8emffqk3nidcrf42mih6impf.apps.googleusercontent.com"
        org-gcal-client-secret "yWPabdIi1ce7RkHlFxlR8GDO"
        org-gcal-file-alist '(("sk8ingdom@gmail.com" .  "c:/users/dominics/Documents/org/gcal.org"))
        org-gcal-auto-archive nil
        org-gcal-logo nil
        org-gcal-icon-list nil))

;; Calfw
(require 'calfw)
(require 'calfw-org)
(setq cfw:org-overwrite-default-keybinding t)

(global-set-key (kbd "C-x c") 'cfw:open-org-calendar)

;; Org-journal
(require 'org-journal)
;; (setq org-journal-dir "~/org/journal")
(setq org-journal-dir "C:/Users/dominics/Dropbox/org/journal")
;; (setq org-journal-enable-agenda-integration t)

;; Swiper/Counsel/Ivy
(define-key org-mode-map (kbd "C-c j") 'counsel-org-goto-all)

(require 'counsel-imdb)
