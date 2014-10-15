;; Enable org-mode
(require 'org)

;; Enable calcfw-org
;; (load "~/.emacs.d/add-ins/calcfw-org")

;; Enable htmlize
(load "~/.emacs.d/add-ins/htmlize")

;; Enable ob-calc
(load "~/.emacs.d/add-ins/ob-calc")

;; Enable org-mobile
(load "~/.emacs.d/add-ins/org-mobile")

;; Enable org-collector
(load "~/.emacs.d/add-ins/org-collector.el")

;; Enable org-protocol
(require 'org-protocol)

;; Enable request
;; (load "~/.emacs.d/add-ins/request.el")

;; Fetch metadata from readability json
(require 'json)
(require 'url)

(defun my/get-and-parse-json (url key)
  (interactive)
  (with-current-buffer
      (url-retrieve-synchronously 
       (concat "http://www.readability.com/api/content/v1/parser?url=" url "&token=b661b54be0fbd228e0bad2854238a3eec30e96b1"))
    (goto-char url-http-end-of-headers)
    (let ((json-object-type 'plist)
	  (json-array-type 'list)
	  (json-key-type 'keyword))
      (let ((result (json-read)))
	(plist-get result (intern (concat ":" key))))
	)))

;; Helper function to remove bad encoding from readability
;; Look up codes here: http://www.danshort.com/HTMLentities/
(defun my/fix-readability-encoding (string)
  (if (equal string nil)
      ""
    (replace-regexp-in-string
     (regexp-quote "&#x201D;") "\""
     (replace-regexp-in-string
      (regexp-quote "&#x201C;") "\""
      (replace-regexp-in-string
       (regexp-quote "&#x2014;") "--"
       (replace-regexp-in-string
	(regexp-quote "&#x2018;") "'"
	(replace-regexp-in-string
	 (regexp-quote "&#x2019;") "'"
	 (replace-regexp-in-string
	  (regexp-quote "&#x2022;") "-"
	  (replace-regexp-in-string
	   (regexp-quote "&#x2026;") "..."
	   (replace-regexp-in-string
	    (regexp-quote "&hellip;") "..."
	    string))))))))))

;; Helper functions for org-capture using org-protocol
(defun my/get-creator()
  (my/fix-readability-encoding
   (my/get-and-parse-json (plist-get org-store-link-plist :link) "author")))

(defun my/get-title()
  (my/fix-readability-encoding
   (my/get-and-parse-json (plist-get org-store-link-plist :link) "title")))

(defun my/get-source()
  (my/fix-readability-encoding
   (my/get-and-parse-json (plist-get org-store-link-plist :link) "domain")))

(defun my/get-via()
  "%^(Via)p")

(defun my/get-date()
  (with-temp-buffer
    (insert
     (replace-regexp-in-string 
      (regexp-quote "[]") "" 
      (concat "[" (my/get-and-parse-json (plist-get org-store-link-plist :link) "date_published") "]")))
    (point-min)
    ;; (message (buffer-string))
    (org-time-stamp-inactive)
    ;; (execute-kbd-macro [return])
    (buffer-string)
    ))

(defun my/get-note()
  (my/fix-readability-encoding
   (my/get-and-parse-json (plist-get org-store-link-plist :link) "excerpt")))

(defun my/get-quote()
  (let ((initial (plist-get org-store-link-plist :initial)))
    (if (equal initial "")
	""
      (concat "\n\n  %?\n\n  #+BEGIN_QUOTE\n  " initial "\n  #+END_QUOTE"))))

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
