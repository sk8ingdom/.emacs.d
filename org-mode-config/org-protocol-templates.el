;; Requires json and url packages
(require 'json)
(require 'url)

(defvar org-kimono-api-url "https://www.kimonolabs.com/api/ondemand/")
(defvar org-kimono-api-key "8d576e98db81c2d0b94202953e69b591")
(defvar org-readability-api-url "http://www.readability.com/api/content/v1/parser?url=")
(defvar org-readability-api-key "b661b54be0fbd228e0bad2854238a3eec30e96b1")

(defun org-multimedia-capture (&optional goto)
  (interactive "P")
  ;; Used to capture multimedia links using Kimono and Readability APIs
  ;; Should be invoked interactively with M-x org-multimedia-capture
  ;; The prompt should be answered with either an encoded link such as:
  ;;     http%3A%2F%2Fwww.wired.com%2F2014%2F10%2Fvolvo-turbo-engine-concept%2F/
  ;; or a Link / quote pair such as:
  ;;     http%3A%2F%2Fwww.wired.com%2F2014%2F10%2Fvolvo-turbo-engine-concept%2F/quote
  ;; This is likely easiest accomplished by creating a javascript bookmarklet such as:
  ;; javascript:(
  ;;     function(s){
  ;;         try{
  ;;             s=document.selection.createRange().text
  ;;         }
  ;;         catch(_){
  ;;             s=document.getSelection()
  ;;         }
  ;;         prompt('',encodeURIComponent(location.href)+
  ;;                '/'+
  ;;                encodeURIComponent(s)
  ;;               )
  ;;     }
  ;; )()
  ;; Like org-capture, using the C-0, C-1, etc. goto prefix arguments work
  ;; The keys prefix argument is not implemented because I don't use it.
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
         (json (get-json link))
         ;; Get the host of the json url based on the link
         (host (url-host (url-generic-parse-url (get-json-url link))))
         ;; Get the parsed data
         ;; Returned as an alist: creator, created, via, source, link, date, note, quote
         (json-data (get-json-data json host))
         (creator (plist-get json-data :creator))
         (created (plist-get json-data :created))
         (date (plist-get json-data :date))
         (note (plist-get json-data :note))
         ;; Prompt for the via link; orglink is created automatically from the link and description prompt
         (via (replace-regexp-in-string
               (regexp-quote "[[][]]") "" (concat "[[" (read-string "Via link: ") "][" (read-string "Via description: ") "]]")))
         ;; Get source from json object
         (source (url-host (url-generic-parse-url link)))
         (orglink (org-make-link-string
                   link (if (string-match "[^[:space:]]" created) created link)))
         ;; avoid call to org-store-link
         (org-capture-link-is-already-stored t))
    ;; Make orglink
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
    ;; (funcall 'org-capture goto "mr")))
    (funcall 'org-capture goto)))

(defun get-json (url)
  ;; Retrieves json object from any URL
  (with-current-buffer (url-retrieve-synchronously (get-json-url url))
    (goto-char url-http-end-of-headers)
    (let ((json-object-type 'plist)
          (json-array-type 'list)
          (json-key-type 'keyword))
      (json-read))))

(defun get-json-url (url)
  ;; Builds the json url from the actual url
  ;; Some domains have specific json URLs
  ;; If the domain doesn't have a specific json URL, it uses the readability API
  (cond ((string-match "imdb\.com" url)
         (concat org-kimono-api-url "5havjcjc?apikey=" org-kimono-api-key "&kimpath2="
                 (caddr (split-string (url-filename (url-generic-parse-url url)) "/"))
                 "&kimwithurl=1"))
        ((string-match "stackoverflow\.com" url)
         (concat org-kimono-api-url "6a74l7lo?apikey=" org-kimono-api-key "&kimpath2="
                 (caddr (split-string (url-filename (url-generic-parse-url url)) "/"))
                 "&kimwithurl=1"))
        ((string-match "emacs\.stackexchange\.com" url)
         (concat org-kimono-api-url "b15iltrs?apikey=" org-kimono-api-key "&kimpath2="
                 (caddr (split-string (url-filename (url-generic-parse-url url)) "/"))
                 "&kimwithurl=1"))
        ((string-match "gmane\.org" url)
         (concat org-kimono-api-url "9oqm87li?apikey=" org-kimono-api-key "&kimpath2="
                 (caddr (split-string (url-filename (url-generic-parse-url url)) "/"))
                 "&kimwithurl=1"))
        ((string-match "amazon\.com" url)
         (concat org-kimono-api-url "8shfuve2?apikey=" org-kimono-api-key "&kimpath2="
                 (caddr (split-string (url-filename (url-generic-parse-url url)) "/"))
                 "&kimwithurl=1"))
        ((string-match "boardgamegeek\.com" url)
         (concat org-kimono-api-url "4iotky3s?apikey=" org-kimono-api-key "&kimpath2="
                 (caddr (split-string (url-filename (url-generic-parse-url url)) "/"))
                 "&kimpath3="
                 (car (last (split-string (url-filename (url-generic-parse-url url)) "/")))
                 "&kimwithurl=1"))
        ((string-match "reddit\.com" url)
         ;; (print
         (concat org-kimono-api-url "9scn1mzk?apikey=" org-kimono-api-key "&kimpath1=r&kimpath2="
                 (nth 2 (split-string (url-filename (url-generic-parse-url url)) "/"))
                 "&kimpath3=comments&kimpath4="
                 (nth 4 (split-string (url-filename (url-generic-parse-url url)) "/"))
                 "&kimpath5="
                 (nth 5 (split-string (url-filename (url-generic-parse-url url)) "/"))
                 "&kimwithurl=1"))
        ;;)
        ((string-match "youtube\.com" url)
         (concat org-kimono-api-url "5qmwewqs?apikey=" org-kimono-api-key "&kimpath1=watch&"
                 (cadr (split-string (url-filename (url-generic-parse-url url)) "?"))
                 "&kimwithurl=1"))
        ((string-match "news\.ycombinator\.com" url)
         (concat "https://hacker-news.firebaseio.com/v0/item/"
                 (cadr (split-string (url-filename (url-generic-parse-url url)) "="))
                 ".json?print=pretty"))
        (t
         (concat org-readability-api-url url "&token=" org-readability-api-key))))

(defun get-json-date-from-org (date)
  ;; Attempts to build the org-date from parsed website data
  (with-temp-buffer
    (insert
     (replace-regexp-in-string
      (regexp-quote "[]") ""
      (concat "[" (org-read-date nil nil date) "]")))
    (point-min)
    (org-time-stamp-inactive)
    (buffer-string)))

(defun get-json-data (json host)
  (if (equal host "www.kimonolabs.com")
      (get-json-data-kimono json)
    (get-json-data-readability json)))

(defun get-json-data-kimono (json)
  (let (;; Get creator from json object
        (creator (or (concat "[["
                             (plist-get (plist-get (car (plist-get (plist-get json :results) :multimedia)) :creator) :href)
                             "]["
                             (plist-get (plist-get (car (plist-get (plist-get json :results) :multimedia)) :creator) :text)
                             "]]")
                     ""))
        ;; Get created from json object
        (created (or (plist-get (car (plist-get (plist-get json :results) :multimedia)) :created) ""))
        ;; Get date from json object; if doesn't exist, set it to nothing
        (date (or (get-json-date-from-org (plist-get (car (plist-get (plist-get json :results) :multimedia)) :date)) ""))
        ;; Get note from json object
        (note (or (plist-get (car (plist-get (plist-get json :results) :multimedia)) :note) "")))
    (setq json-data nil)
    (setq json-data (plist-put json-data :creator creator))
    (setq json-data (plist-put json-data :created created))
    (setq json-data (plist-put json-data :date date))
    (setq json-data (plist-put json-data :note note))))

(defun get-json-data-readability (json)
  (let (;; Get creator from json object
        (creator (fix-encoding (plist-get json :author)))
        ;; Get created from json object
        (created (fix-encoding (plist-get json :title)))
        ;; Get date from json object; if doesn't exist, set it to nothing
        (date (get-json-date-from-org (plist-get json :date_published)))
        ;; Get note from json object
        (note (fix-encoding (plist-get json :excerpt))))
    (setq json-data nil)
    (setq json-data (plist-put json-data :creator creator))
    (setq json-data (plist-put json-data :created created))
    (setq json-data (plist-put json-data :date date))
    (setq json-data (plist-put json-data :note note))))

(defun fix-encoding (string)
  ;; Helper function to remove bad encoding from readability
  ;; Look up codes here: http://www.danshort.com/HTMLentities/
  (if (equal string nil)
      ""
    (replace-regexp-in-string
     (regexp-quote "&quot;") "\""
     (replace-regexp-in-string
      (regexp-quote "&#xA0;") " "
      (replace-regexp-in-string
       (regexp-quote "&amp;") "&"
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
               (replace-regexp-in-string
                (regexp-quote "’") "'"
                (replace-regexp-in-string
                 (regexp-quote "–") "--"
               string)))))))))))))))
