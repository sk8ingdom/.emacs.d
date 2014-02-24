;; org-mode configuration
;; Enable org-mode
(require 'org)

;; Make org-mode work with files ending in .org
;; Default in recent emacsen
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Disable subscripts on export
(setq org-export-with-sub-superscripts nil)

;; Shortcuts
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Property Inheritance
(setq org-use-property-inheritance t)

;; Use global IDs
(require 'org-id)
(setq org-id-link-to-org-use-id t)

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
