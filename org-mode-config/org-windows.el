;; Capture
(setq org-directory "U:/org")
(setq org-default-notes-file (concat org-directory "/ref.org"))

;; Targets that contribute to the agenda view
(setq org-agenda-files (quote ("U:/org")))

;; Abbreviations
(add-to-list 'org-link-abbrev-alist '("local"   . "U:/"))
(add-to-list 'org-link-abbrev-alist '("val"     . "https://dominicsurano.com:8081/homes/sk8ingdom/"))
;; (add-to-list 'org-link-abbrev-alist '("outlook" . "C:/Program Files (x86)/Microsoft Office/Office14/OUTLOOK.EXE /select Outlook:"))

;; Plugins

;; Enable org-outlook
(require 'org-outlook)
