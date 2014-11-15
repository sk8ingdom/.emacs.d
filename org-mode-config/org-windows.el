;; Capture
(setq org-directory "U:/org")
(setq org-default-notes-file (concat org-directory "/ref.org"))
(define-key global-map "\C-cc" 'org-capture)

;; Targets that contribute to the agenda view
(setq org-agenda-files (quote ("U:/org")))

;; Abbreviations
(add-to-list 'org-link-abbrev-alist '("local"   . "U:/"))
(add-to-list 'org-link-abbrev-alist '("val"     . "http://dominicsurano.com:4041/homes/sk8ingdom/"))
;; (add-to-list 'org-link-abbrev-alist '("outlook" . "C:/Program Files (x86)/Microsoft Office/Office14/OUTLOOK.EXE /select Outlook:"))

;; Plugins

;; Enable org-outlook
(load "~/.emacs.d/plugins/org-outlook")
