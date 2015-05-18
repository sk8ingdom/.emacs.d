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
(setq org-export-with-drawers t)

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
