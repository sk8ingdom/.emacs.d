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
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cv" 'org-reveal)

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
;; Added for [[id:2d61b197-2652-44e5-88f4-70f31e2bcf07]]
(defun org-dired-store-link ()
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
(org-link-set-parameters "dired" :store 'org-dired-store-link)

;; Have dired store org-link when a file or files(s) are renamed
(defun my/dired-rename-file (file newname ok-if-already-exists)
  "Store org-link to the renamed file."
  (push (list newname (file-name-nondirectory newname)) org-stored-links))

(advice-add #'dired-rename-file :after #'my/dired-rename-file)

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
(defun my/freeze-time ()
  "Freeze `current-time' at the current active or inactive timestamp. If point
is not on a timestamp, the function prompts for one. If time is not specified,
either by the timstamp under point or prompt, the time defaults to the
current HH:MM of today at the selected date."
  (interactive)
  (let ((time
         (cond ((org-at-timestamp-p t)
                (match-string 0))
               (t
                (org-read-date t nil nil "Input freeze time:")))))
    (eval (macroexpand
           `(defadvice current-time (around freeze activate)
              (setq ad-return-value ',
                    (append (org-read-date nil t time) '(0 0))))))
    (set-face-background 'fringe "firebrick2")))

(global-set-key "\C-cf" 'my/freeze-time)

;; Release changed / frozen time
(defun my/release-time ()
  "Release the time frozen by `freeze-time'."
  (interactive)
  (ad-remove-advice 'current-time 'around 'freeze)
  (ad-activate 'current-time)
  (set-face-background 'fringe nil))

(global-set-key "\C-cr" 'my/release-time)

;; Change time-stamp increments to 1 minute
(setq org-time-stamp-rounding-minutes '(0 1))

;; Create abbreviations
(defun my/create-org-link-abbreviations ()
  "Replace all long form links in current file with their corresponding abbreviations in `org-link-abbrev-alist'."
  (interactive)
  (dolist (pair org-link-abbrev-alist)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward (concat "[[" (cdr pair)) nil t)
        (replace-match (concat "[[" (car pair) ":"))))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'write-file-functions 'my/create-org-link-abbreviations)))

;; Remove abbreviations
(defun my/remove-org-link-abbreviations ()
  "Replace all link abbreviations in current file with their long form counterparts in `org-link-abbrev-alist'."
  (interactive)
  (dolist (pair org-link-abbrev-alist)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward (concat "[[" (car pair) ":") nil t)
        (replace-match (concat "[[" (cdr pair)))))))

;; Re-define org-switch-to-buffer-other-window to NOT use org-no-popups.
;; Primarily for compatibility with shackle.
(defun org-switch-to-buffer-other-window (args)
  "Switch to buffer in a second window on the current frame.
In particular, do not allow pop-up frames.
Returns the newly created buffer.

Redefined to allow pop-up windows."
  ;;  (org-no-popups
  ;;     (apply 'switch-to-buffer-other-window args)))
  (switch-to-buffer-other-window args))

;; Org-toggle-latex-fragment options

;; Make text bigger
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

;; Semi-center equations by moving number to the right using [leqno]
(setq org-format-latex-header "\\documentclass[reqno]{article}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")
