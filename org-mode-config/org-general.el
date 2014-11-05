;; Enable org-mode
(require 'org)

;; Plugins

;; Enable calcfw-org
;; (load "~/.emacs.d/plugins/calcfw-org")

;; Enable ob-calc
(load "~/.emacs.d/plugins/ob-calc")

;; Enable org-mobile
(load "~/.emacs.d/plugins/org-mobile")

;; Enable org-collector
(load "~/.emacs.d/plugins/org-collector")

;; Enable org-protocol
(require 'org-protocol)
(load "~/.emacs.d/org-mode-config/org-protocol-templates")

;; Enable org-outlook
(load "~/.emacs.d/plugins/org-outlook")

;; Enable syntax-highlighting
(setq org-src-fontify-natively t)

;; Disable subscripts on export
(setq org-export-with-sub-superscripts nil)

;; Shortcuts
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Property inheritance
(setq org-use-property-inheritance nil)

;; Tag inheritance
(setq org-use-tag-inheritance nil)

;; Use global IDs
(require 'org-id)
(setq org-id-link-to-org-use-id t)

;; Update ID file .org-id-locations on startup
(org-id-update-id-locations)

;; Add ID automatically on capture
(add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)

;; Add ID properties to all headlines in the current file which do not already have one
(defun my/org-add-ids-to-headlines-in-file ()
  (interactive)
  (org-map-entries 'org-id-get-create))

;; Uncomment to enable writing IDs to all org entries before saving
;; (add-hook 'org-mode-hook
;;  (lambda ()
;;    (add-hook 'before-save-hook 'my/org-add-ids-to-headlines-in-file nil 'local)))

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps t)
(setq org-refile-allow-creating-parent-nodes t)

;; Abbreviations
(setq org-link-abbrev-alist
       '(
	 ;; Abbreviations for files
	 ("dev"            . "file:dev.org::*")
	 ("edu"            . "file:edu.org::*")
	 ("fin"            . "file:fin.org::*")
	 ("hea"            . "file:hea.org::*")
	 ("hob"            . "file:hob.org::*")
	 ("mul"            . "file:mul.org::*")
	 ("org"            . "file:org.org::*")
	 ("peo"            . "file:peo.org::*")
	 ("pos"            . "file:pos.org::*")
	 ("ref"            . "file:ref.org::*")
	 ("soc"            . "file:soc.org::*")
	 ("wrk"            . "file:wrk.org::*")

	 ;; Abbreviations for websites
         ("facebook"       . "https://www.facebook.com/")
	 ("fedex"          . "https://www.fedex.com/fedextrack/?tracknumbers=")
         ("gmail"          . "https://mail.google.com/mail/u/0/#all/")
         ("google"         . "https://www.google.com/#q=")
         ("google-maps"    . "https://maps.google.com/maps?q=")
         ("google-plus"    . "https://plus.google.com/")
         ("google-scholar" . "http://scholar.google.com/scholar?hl=en&q=")
         ("github"         . "https://www.github.com/")
         ("hacker-news"    . "https://news.ycombinator.com/item?id=")
         ("instagram"      . "https://www.instagram.com/")
         ("linkedin"       . "http://www.linkedin.com/")
         ("okcupid"        . "http://www.okcupid.com/profile/")
         ("pinterest"      . "http://www.pinterest.com/")
         ("reddit"         . "http://reddit.com/user/")
         ("twitter"        . "https://www.twitter.com/")
	 ("ups"            . "http://www.ups.com/WebTracking/processRequest?tracknum=")
	 ("usps"           . "https://tools.usps.com/go/TrackConfirmAction.action?tRef=fullpage&tLc=1&text28777=&tLabels=")
         ("yelp-business"  . "http://www.yelp.com/biz/")
         ("yelp-user"      . "http://www.yelp.com/user_details?userid=")
         ("youtube"        . "http://www.youtube.com/user/")
	 )
       )

;; Export drawers
(setq org-export-with-drawers t)
