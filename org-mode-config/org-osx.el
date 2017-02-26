;; Capture
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/ref.org"))

;; Targets that contribute to the agenda view
(setq org-agenda-files (quote ("~/org")))

;; Abbreviations
;; (if (string-equal "[192 168 1 127 0]" (format "%s" (cdar (network-interface-list))))
;;     (add-to-list 'org-link-abbrev-alist '("val"     . "/Volumes/home/"))
;;   (add-to-list 'org-link-abbrev-alist '("val"     . "/Volumes/sk8ingdom/")))
(add-to-list 'org-link-abbrev-alist '("val"     . "/Volumes/home/"))

;; Org-mobile
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg/")
