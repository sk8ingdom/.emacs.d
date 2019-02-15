;; Check org-link-parameters

;;;; Dired

(require 'dired)

;; Added for [[id:2d61b197-2652-44e5-88f4-70f31e2bcf07]]
(defun my/org-dired-store-link ()
  "Store a link to the file at point in a dired buffer. Shortens default dired link to file name"
  (when (derived-mode-p 'dired-mode)
    (let ((file (dired-get-filename nil t)))
      (setf file (if file
                     (abbreviate-file-name (expand-file-name file))
                   default-directory))
      (org-store-link-props :type        "dired"
                            :link        file
                            :description (file-name-nondirectory file))
      file)))

;; Adds dired link to org-link-parameters variable, replaces org-add-link-type
(org-link-set-parameters "dired" :store 'my/org-dired-store-link)

(advice-add #'dired-rename-file :after #'my/dired-rename-file)

;;;; Help

(require 'help)

(defun my/org-help-store-link ()
  "Store a link to an help buffer."
  (when (eq major-mode 'help-mode)
    (let* ((description  (save-excursion
                    (goto-char (point-min))
                    (looking-at "^[^ ]+")
                    (match-string 0)))
           (link (concat "help:" description)))
      (org-store-link-props :type "help"
                            :link link
                            :description description)
      link)))

(org-link-set-parameters "help" :store 'my/org-help-store-link)


;;;; Any Programming File

;; (defun my/org-file-store-link ()
;;   "Store a link to a programming file."
;;   (when (eq major-mode 'emacs-lisp-mode
;;             (let ((description (abbreviate-file-name

;;;; Org-mode Utility Functions

;; Remove all links from org-stored-links
(defun my/org-stored-links-remove ()
  (interactive)
  (if org-stored-links
      (let ()
        (setq org-stored-links nil)
        (message "All links removed from org-stored-links."))
    (message "No links in org-stored-links.")))

(global-set-key "\C-cd" 'my/org-stored-links-remove)

;; Force links to open in current window
(setq org-link-frame-setup
      (quote
       ((file . find-file))))

;; Prompt for path completion when an id type link when org-insert-link is called
;; Use with C-c C-l id: RET
(defun my/org-id-complete-link (&optional arg)
  "Create an id: link using completion"
  (concat "id:"
          (my/org-id-get-with-outline-path-completion org-refile-targets)))

;; Add function to insert all org-links as a bulleted or comma separated list
(require 'with-simulated-input)

(defun my/org-insert-link-all ()
  (interactive)
  (while org-stored-links
    (with-simulated-input "RET RET"
      (org-insert-link))
    (if (last org-stored-links)
        (progn
          (if (org-in-item-p)
              (org-insert-item)
            (insert ", "))))))

;; Remove link and retain description
(defun my/org-replace-link-by-link-description ()
  "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (let ((remove (list (match-beginning 0) (match-end 0)))
            (description (if (match-end 3)
                             (org-match-string-no-properties 3)
                           (org-match-string-no-properties 1))))
        (apply 'delete-region remove)
        (insert description))))

;; Insert contact
(defun my/org-insert-link-contact (&optional contact)
  (interactive)
  (if (equal contact nil)
      (progn
        (setq contact (read-string "Person or business: "))
        (insert (org-make-link-string (concat "peo:" contact) contact)))
    (org-make-link-string (concat "peo:" contact) contact)))

(define-key org-mode-map "\C-cg" 'my/org-insert-link-contact)

;; Insert multiple contacts
;; Requires list in the format ("Dominic Surano" "Other Person")
(defun my/org-insert-link-contacts (&optional contacts)
  (let ((result ""))
    (while contacts
      (setq result (concat result (my/org-insert-link-contact (car contacts))))
      (if (> (length contacts) 1)
          (setq result (concat result ", ")))
      (setq contacts (cdr contacts)))
    result))
