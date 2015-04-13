;; Capture
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/ref.org"))

;; Targets that contribute to the agenda view
(setq org-agenda-files (quote ("~/org")))

;; Abbreviations
(add-to-list 'org-link-abbrev-alist '("val"     . "/Volumes/home/"))
(add-to-list 'org-link-abbrev-alist '("local"   . "~/"))
