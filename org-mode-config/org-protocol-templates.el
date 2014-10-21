;; Require json and url packages
(require 'json)
(require 'url)

(defun org-protocol-capture-readability ()
  (interactive)
  ;; Used to capture multimedia links using the readability API
  ;; Should be invoked interactively with M-x org-protocol-capture-readability
  ;; The prompt should be answered with either an encoded link such as:
  ;;     http%3A%2F%2Fwww.wired.com%2F2014%2F10%2Fvolvo-turbo-engine-concept%2F/
  ;; or a Link / quote pair such as:
  ;;     http%3A%2F%2Fwww.wired.com%2F2014%2F10%2Fvolvo-turbo-engine-concept%2F/quote
  ;; This is likely easiest accomplished by creating a javascript bookmarklet such as:
  ;;     javascript:(function(s){try{s=document.selection.createRange().text}catch(_){s=document.getSelection()}prompt('',encodeURIComponent(location.href)+'/'+encodeURIComponent(s))})()
  ;; and copying the text
  (let* (
	 ;; Prompt for the data which should be in the form describe above
	 (data (read-string "Link / quote: "))
	 ;; Split data into parts based on the org-protocol-data-separator, which by default is "/"
	 (parts (org-protocol-split-data data t org-protocol-data-separator))
	 ;; Separate out the link which is the first part of parts and sanitize it
	 (link (org-protocol-sanitize-uri (car parts)))
	 ;; Separate out the quote which is the second part of parts; if it doesn't exist, set it to nothing
	 (quote (if (equal (cadr parts) (or nil ""))
		    ""
		  (concat "\n\n  %?\n\n  #+BEGIN_QUOTE\n  " (cadr parts) "\n  #+END_QUOTE")))
	 ;; Get the json object based on the link
	 (json (get-json-readability link))
	 ;; Get creator from json object
	 (creator (or (fix-encoding-readability (plist-get json :author)) ""))
	 ;; Get created from json object
	 (created (or (fix-encoding-readability (plist-get json :title)) ""))
	 ;; Prompt for the via link; orglink is created automatically from the link and description prompt
	 (via (or (concat "[[" (read-string "Via link: ") "][" (read-string "Via description: ") "]]") ""))
	 ;; Get source from json object
	 (source (or (fix-encoding-readability (plist-get json :domain)) ""))
	 ;; Get date from json object; if doesn't exist, set it to nothing
	 (date (if (plist-get json :date_published)
		   (with-temp-buffer
		     (insert
		      (replace-regexp-in-string 
		       (regexp-quote "[]") "" 
		       (concat "[" (plist-get json :date_published) "]")))
		     (point-min)
		     (org-time-stamp-inactive)
		     (buffer-string))
		   ""))
	 ;; Get note from json object
	 (note (or (fix-encoding-readability (plist-get json :excerpt)) ""))
	 ;; Make orglink
	 (orglink (org-make-link-string
	 	   link (if (string-match "[^[:space:]]" created) created link)))
	 (org-capture-link-is-already-stored t)) ;; avoid call to org-store-link
    (setq org-stored-links
	  (cons (list link created) org-stored-links))
    (kill-new orglink)
    (org-store-link-props :creator creator
	 		  :created created
			  :via via
	 		  :source source
	 		  :link link
	 		  :date date
	 		  :note note
	 		  :quote quote
	 		  )
    (raise-frame)
    (funcall 'org-capture)))

;; Legacy functions
;; All functions below this point were created before I had a good idea what I was doing
;; They will likely be deleted in the future but currently still work

(defun get-json-readability (url)
  (interactive)
  ;; http://www.readability.com/api/content/v1/parser?url=
  ;; &token=b661b54be0fbd228e0bad2854238a3eec30e96b1
  (with-current-buffer (url-retrieve-synchronously (concat "http://www.readability.com/api/content/v1/parser?url=" url "&token=b661b54be0fbd228e0bad2854238a3eec30e96b1"))
    (goto-char url-http-end-of-headers)
    (let ((json-object-type 'plist)
	  (json-array-type 'list)
	  (json-key-type 'keyword))
      (json-read))))

;; Fetch metadata from readability json
;; Works with the following capture template:
;;  ("mf" "REFERENCE (f) Reference org-protocol" entry (file "ref.org")
;;   "* REFERENCE [[%:link][%(get-title-readability)]]\n  CREATED: %U
;; :PROPERTIES:
;; :Creator:  %(get-creator-readability)
;; :Created:  %(get-title-readability)
;; :Source:   %(get-source-readability)
;; :Via:      %(get-via-readability)
;; :Link:     %:link
;; :Date:     %(get-date-readability)
;; :Note:     %(get-note-readability)
;; :END:      %(get-quote-readability)")

(defun get-and-parse-json-readability (url key)
  (interactive)
  (with-current-buffer (url-retrieve-synchronously (concat "http://www.readability.com/api/content/v1/parser?url=" url "&token=b661b54be0fbd228e0bad2854238a3eec30e96b1"))
    (goto-char url-http-end-of-headers)
    (let ((json-object-type 'plist)
	  (json-array-type 'list)
	  (json-key-type 'keyword))
      (let ((result (json-read)))
	(plist-get result (intern (concat ":" key)))))))

(defun fix-encoding-readability (string)
  ;; Helper function to remove bad encoding from readability
  ;; Look up codes here: http://www.danshort.com/HTMLentities/
  (if (equal string nil)
      ""
    (replace-regexp-in-string
     (regexp-quote  "&amp;") "&"
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
	    string)))))))))))

;; Helper functions for org-capture using org-protocol
(defun get-creator-readability()
  (fix-encoding-readability
   (get-and-parse-json-readability (plist-get org-store-link-plist :link) "author")))

(defun get-title-readability()
  (fix-encoding-readability
   (get-and-parse-json-readability (plist-get org-store-link-plist :link) "title")))

(defun get-source-readability()
  (fix-encoding-readability
   (get-and-parse-json-readability (plist-get org-store-link-plist :link) "domain")))

(defun get-via-readability()
  (with-temp-buffer
    (org-insert-link)
    (buffer-string)))

(defun get-date-readability()
  (with-temp-buffer
    (insert
     (replace-regexp-in-string 
      (regexp-quote "[]") "" 
      (concat "[" (get-and-parse-json-readability (plist-get org-store-link-plist :link) "date_published") "]")))
    (point-min)
    (org-time-stamp-inactive)
    (buffer-string)))

(defun get-note-readability()
  (fix-encoding-readability
   (get-and-parse-json-readability (plist-get org-store-link-plist :link) "excerpt")))

(defun get-quote-readability()
  (let ((initial (plist-get org-store-link-plist :initial)))
    (if (equal initial "")
	""
      (concat "\n\n  %?\n\n  #+BEGIN_QUOTE\n  " initial "\n  #+END_QUOTE"))))
