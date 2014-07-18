;; Capture
(setq org-directory "/share/sk8ingdom/org")
(setq org-default-notes-file (concat org-directory "/ref.org"))
(define-key global-map "\C-cc" 'org-capture)

;; Targets that contribute to the agenda view
(setq org-agenda-files (quote ("/share/homes/sk8ingdom")))

;; Abbreviations
(add-to-list 'org-link-abbrev-alist '("local" ."~/"))
(add-to-list 'org-link-abbrev-alist '("val"   . "TBA"))
