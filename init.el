;; -*- mode: elisp -*-

 ;; General
(load "~/.emacs.d/general-config/general")

;; Org-mode
(load "~/.emacs.d/org-mode-config/general-org")
(load "~/.emacs.d/org-mode-config/to-do-states")
(load "~/.emacs.d/org-mode-config/capture-templates")
(load "~/.emacs.d/org-mode-config/agenda-custom")

(cond
 ;; Windows
 ((string-equal system-type "windows-nt")
  (load "~/.emacs.d/windows-config/windows")
  (load "~/.emacs.d/org-mode-config/windows-org")
 )
 ;; OSX
 ((string-equal system-type "darwin")
  (load "~/.emacs.d/osx-config/osx")
  (load "~/.emacs.d/org-mode-config/osx-org")
 )
 ;; Linux
 ((string-equal system-type "gnu/linux")
  (load "~/.emacs.d/linux-config/linux")
  (load "~/.emacs.d/org-mode-config/linux-org")
 )
)

;; Plugins
;; (load "~/.emacs.d/add-ins/request.el")


;; json reader
(require 'json)
(require 'url)

;; http://stackoverflow.com/questions/25692558/how-to-hook-emacs-up-to-a-json-service
(defun get-and-parse-json (url)
  (interactive)
  (url-retrieve
   (concat "http://www.readability.com/api/content/v1/parser?url=" url "&token=b661b54be0fbd228e0bad2854238a3eec30e96b1")
   (lambda (events)
     (goto-char url-http-end-of-headers)
     (let ((json-object-type 'plist)
	   (json-array-type 'list)
	   (json-key-type 'keyword))
       (let ((result (json-read)))
	 (message "Title is: %s" (plist-get result :title))
	 (message "Date is: %s" (plist-get result :date_published))
	 (message "URL is: %s" (plist-get result :url))
	 (message "Author is: %s" (plist-get result :author))
	 )))))
