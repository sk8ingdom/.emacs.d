;; Enable org-mode
(require 'org)
(require 'org-crypt)

;; Remove trailing whitespace
(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; Enable intelligent text wrapping
(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode)
            (adaptive-wrap-prefix-mode)))

;; Enable syntax-highlighting
(setq org-src-fontify-natively t)

;; Disable subscripts on export
(setq org-export-with-sub-superscripts nil)

;; Shortcuts
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(define-key global-map "\C-cc" 'org-capture)

;; Property inheritance
(setq org-use-property-inheritance nil)

;; Tag inheritance
(setq org-use-tag-inheritance nil)

;; Use global IDs
(require 'org-id)
(setq org-id-link-to-org-use-id t)

;; Update ID file .org-id-locations on startup
(org-id-update-id-locations)

;; Add ID properties to all headlines in the current file which do not already have one
(defun my/org-add-ids-to-headlines-in-file ()
  (interactive)
  (org-map-entries 'org-id-get-create))

;; Uncomment to enable writing IDs to all org entries before saving
;; (add-hook 'org-mode-hook
;;  (lambda ()
;;    (add-hook 'before-save-hook 'my/org-add-ids-to-headlines-in-file nil 'local)))

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets '((org-agenda-files :maxlevel . 9)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps t)
(setq org-refile-allow-creating-parent-nodes t)

;; Export drawers
;; (setq org-export-with-drawers t)

;; Collapse plain lists
(setq org-cycle-include-plain-lists 'integrate)

;; Force links to open in current window
(setq org-link-frame-setup
      (quote
       ((file . find-file))))

;; Prompt for path completion when an id type link when org-insert-link is called
;; Use with C-c C-l id: RET
(defun org-id-complete-link (&optional arg)
  "Create an id: link using completion"
  (concat "id:"
          (org-id-get-with-outline-path-completion org-refile-targets)))

;; Shorten dired links to file name
;; Added for  [[id:2d61b197-2652-44e5-88f4-70f31e2bcf07]]
(defun dired-store-link ()
  (when (derived-mode-p 'dired-mode)
    (let ((file (dired-get-filename nil t)))
      (setf file (if file
                     (abbreviate-file-name (expand-file-name file))
                   default-directory))
      (org-store-link-props :type        "dired"
                            :link        file
                            :description (file-name-nondirectory file))
      file)))

(add-to-list 'org-store-link-functions 'dired-store-link)

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

;; Change and freeze time
(defun my/freeze-time (time)
  "Freeze `current-time' at the given TIME"
  (interactive (list (org-read-date nil nil nil "Input freeze time:")))
  (eval (macroexpand `(defadvice current-time (around freeze activate)
                        (setq ad-return-value ',(append (org-read-date nil t time) '(0 0))))))
  (set-face-background 'fringe "firebrick2"))

;; Release changed / frozen time
(defun my/release-time ()
  "Release the time frozen by `freeze-time'."
  (interactive)
  (ad-remove-advice 'current-time 'around 'freeze)
  (ad-activate 'current-time)
  (set-face-background 'fringe nil))
