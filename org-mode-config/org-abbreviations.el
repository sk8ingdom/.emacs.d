;; Abbreviations
(setq org-link-abbrev-alist
      '(;; Abbreviations for files
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
        ("ontrac"         . "http://www.ontrac.com/trackingres.asp?tracking_number=")
        ("pinterest"      . "http://www.pinterest.com/")
        ("reddit"         . "http://reddit.com/user/")
        ("twitter"        . "https://www.twitter.com/")
        ("ups"            . "http://www.ups.com/WebTracking/processRequest?tracknum=")
        ("usps"           . "https://tools.usps.com/go/TrackConfirmAction.action?tRef=fullpage&tLc=1&text28777=&tLabels=")
        ("yelp-business"  . "http://www.yelp.com/biz/")
        ("yelp-user"      . "http://www.yelp.com/user_details?userid=")
        ("youtube"        . "http://www.youtube.com/user/")
        ;; Abbreviations for other protocols
        ("tel"            . "tel:")))

;; Create abbreviations
(defun my/org-link-abbreviations-create ()
  "Replace all long form links in current file with their corresponding abbreviations in `org-link-abbrev-alist'."
  (interactive)
  (dolist (pair org-link-abbrev-alist)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward (concat "[[" (cdr pair)) nil t)
        (replace-match (concat "[[" (car pair) ":"))))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'write-file-functions 'my/org-link-abbreviations-create)))

;; Remove abbreviations
(defun my/org-link-abbreviations-remove ()
  "Replace all link abbreviations in current file with their long form counterparts in `org-link-abbrev-alist'."
  (interactive)
  (dolist (pair org-link-abbrev-alist)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward (concat "[[" (car pair) ":") nil t)
        (replace-match (concat "[[" (cdr pair)))))))
