;; Capture
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/ref.org"))

;; Targets that contribute to the agenda view
(setq org-agenda-files (quote ("~/org")))

;; Abbreviations
(if (string-equal "[192 168 1 127 0]" (format "%s" (cdar (network-interface-list))))
    (add-to-list 'org-link-abbrev-alist '("val"     . "file+sys:/Volumes/home/"))
  (add-to-list 'org-link-abbrev-alist '("val"     . "file+sys:/Volumes/sk8ingdom/")))
