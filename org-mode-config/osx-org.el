;; Capture
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/ref.org"))
(define-key global-map "\C-cc" 'org-capture)

;; Targets that contribute to the agenda view
(setq org-agenda-files (quote ("~/org")))

;; Abbreviations
(add-to-list 'org-link-abbrev-alist
       '("local"   . "~/")
       ;; Works anywhere
       '("val"     . "Volumes/sk8ingdom/home/")
       ;; Only works when connecte to local home network
       ;;'("val-afp" . "Volumes/home/")
       )
