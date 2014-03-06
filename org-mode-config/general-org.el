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

;; Abbreviations
(setq org-link-abbrev-alist
       '(("dev"            . "file:dev.org::*")
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
         ("facebook"       . "https://www.facebook.com/")
         ("gmail"          . "https://mail.google.com/mail/u/0/#all/")
         ("google"         . "https://www.google.com/#q=")
         ("google-maps"    . "https://maps.google.com/maps?q=")
         ("google-plus"    . "https://plus.google.com/")
         ("google-scholar" . "http://scholar.google.com/scholar?hl=en&q=")
         ("github"         . "https://www.github.com/")
         ("instagram"      . "https://www.instagram.com/")
         ("linkedin"       . "http://www.linkedin.com/")
         ("okcupid"        . "http://www.okcupid.com/profile/")
         ("pinterest"      . "http://www.pinterest.com/")
         ("reddit"         . "http://reddit.com/user/")
         ("twitter"        . "https://www.twitter.com/")
         ("yelp-business"  . "http://www.yelp.com/biz/")
         ("yelp-user"      . "http://www.yelp.com/user_details?userid=")
         ("youtube"        . "http://www.youtube.com/user/")))
