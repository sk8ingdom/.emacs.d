;; Enable org-mode
(require 'org)

;; For encrypting files
(require 'org-crypt)

;; For template expansion
;; https://www.reddit.com/r/orgmode/comments/7jedp0/easy_templates_expansion_not_working/
(require 'org-tempo)

;; Remove trailing whitespace
(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; Enable intelligent text wrapping
(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode)
            (adaptive-wrap-prefix-mode)))

;; Prevent extra spaces from showing up after headings
(setq org-cycle-separator-lines 0)

;; Enable syntax-highlighting
(setq org-src-fontify-natively t)

;;;; Export Options

;; Disable subscripts on export
(setq org-export-with-sub-superscripts nil)

;; Disable table of contents on export
(setq org-export-with-toc nil)

;; Export drawers
;; (setq org-export-with-drawers t)

;; Export to clipboard to paste in other programs
(defun my/org-export-region-html ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil t nil t))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "clip"))
      (kill-buffer buf))))

;; Shortcuts
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(define-key org-mode-map "\C-cv" 'org-reveal)
(define-key org-mode-map "\C-cn" 'org-next-link)
(define-key org-mode-map "\C-cp" 'org-property-action)

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


;; Org-refile

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets '((org-agenda-files :maxlevel . 9)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes t)

;; Add previous heading to refile note automatically
(defun my/org-refile-with-note-parent-link ()
  "Wrapper for `org-refile' which automatically adds the previous
parent org-link to the note in the form \"From [[id:hash][Heading]].\"

Requires `org-log-refile' to be set to 'note."
  (interactive)
  ;; (if (equal current-prefix-arg '(4))
  ;;    (org-refile)
    (save-excursion
      (let ((start-level (funcall outline-level)))
        (if (<= start-level 1)
            ;; Remember that org-make-link-string exists
            (push (list (buffer-file-name)
                        (file-name-nondirectory (buffer-file-name)))
                  org-stored-links)
          (progn
            (outline-up-heading 1 t)
            (call-interactively 'org-store-link)))))
    (org-refile)
    (with-simulated-input "RET RET"
      (org-insert-link))
    (insert ".")
    (beginning-of-line nil)
    (insert "From ")
    (org-ctrl-c-ctrl-c))

(define-key org-mode-map "\C-c\C-w" 'my/org-refile-with-note-parent-link)

;; ;; Has not been implemented for org-agenda-mode-map
;; (define-key org-agenda-mode-map "\C-c\C-w" 'my/org-refile-with-note-parent-link)

(defun my/org-copy-outline-path ()
  "Function to copy outline path of current org-item (agenda or file) to clipboard."
  (interactive)
  (let ((m (org-get-at-bol 'org-marker)))
    (kill-new (replace-regexp-in-string "//" "/" (org-with-point-at m (org-display-outline-path t t "/" nil))))))

(define-key org-mode-map "\C-co" 'my/org-copy-outline-path)

;; Collapse plain lists
(setq org-cycle-include-plain-lists 'integrate)

;; Change and freeze time
(defun my/freeze-time (&optional freeze-time-time)
  "Freeze `current-time' at the current active or inactive timestamp. If point
is not on a timestamp, the function prompts for one. If time is not specified,
either by the timstamp under point or prompt, the time defaults to the
current HH:MM of today at the selected date."
  (interactive)
  (let ((time
         (cond
          ((if (boundp 'freeze-time-time)
               freeze-time-time))
          ((if (org-at-timestamp-p 'lax) t)
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
  (if (advice--p (advice--symbol-function 'current-time))
      (progn
        (ad-remove-advice 'current-time 'around 'freeze)
        (ad-activate 'current-time)
        (set-face-background 'fringe nil))
    (message "Time is not currently frozen")))

(global-set-key "\C-cr" 'my/release-time)

;; Change time-stamp increments to 1 minute
(setq org-time-stamp-rounding-minutes '(0 1))

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

;; Go to the next indented paragraph when currently in a bulleted list.
(defun my/org-next-paragraph ()
  (interactive)
  (move-end-of-line nil)
  (org-return)
  (org-return)
  (org-return)
  (indent-for-tab-command nil)
  (org-cycle nil)
  (org-self-insert-command 1)
  (beginning-of-visual-line 1)
  (previous-line 1 1)
  (delete-forward-char 1 nil)
  (end-of-visual-line 1)
  (org-delete-backward-char 1))

(define-key org-mode-map "\C-ci" 'my/org-next-paragraph)

;; Add note on property change
(add-to-list 'org-log-note-headings
             '(property . "Property %-12s from %-12S %t"))

(defcustom my/org-property-ignored-properties
  '("ID" "LAST_REPEAT" "Via" "ARCHIVE_TIME" "ARCHIVE_FILE" "ARCHIVE_OLPATH" "ARCHIVE_CATEGORY" "ARCHIVE_TODO" "Effort" "EFFORT" "NOTER_DOCUMENT" "NOTER_PAGE")
  "List of properties to exclude from my/org-property-change-note."
  :group 'org
  :type 'list)

(defun my/org-property-store-previous-val (property)
  "Store previous property value prior to modifying it with `org-property-action'."
  (setq my/org-property-previous-val
        (org-entry-get nil property)))

(advice-add #'org-read-property-value :before #'my/org-property-store-previous-val)

(defun my/org-property-change-note (prop val)
;;   "Add property changes to the logbook. Requires modifying `org-add-log-note'
;; to include:

;; ((eq org-log-note-purpose 'property)
;;  (format \"\\\"%s\\\" property change from \\\"%s\\\"\"
;;          (or org-log-note-state \"\")
;;          (or org-log-note-previous-state \"\")))

;; or replacing the entire cond block with:

;; (cond
;;  ((member org-log-note-purpose (mapcar 'car org-log-note-headings))
;;   \"changing property\")
;;  (t (error \"This should not happen\")))

;; and byte compiling org.el."
  (message (concat "Changing " prop " from " val))
  (if (not 'my/org-property-previous-val)
      (if (not (member prop my/org-property-ignored-properties))
      (org-add-log-setup 'property prop my/org-property-previous-val))))

;; In the interim, I've just re-defined the function
(defun org-add-log-note (&optional _purpose)
  "Pop up a window for taking a note, and add this note later."
  (remove-hook 'post-command-hook 'org-add-log-note)
  (setq org-log-note-window-configuration (current-window-configuration))
  (delete-other-windows)
  (move-marker org-log-note-return-to (point))
  (pop-to-buffer-same-window (marker-buffer org-log-note-marker))
  (goto-char org-log-note-marker)
  (org-switch-to-buffer-other-window "*Org Note*")
  (erase-buffer)
  (if (memq org-log-note-how '(time state))
      (org-store-log-note)
    (let ((org-inhibit-startup t)) (org-mode))
    (insert (format "# Insert note for %s.
# Finish with C-c C-c, or cancel with C-c C-k.\n\n"
                    (cond
                     ((eq org-log-note-purpose 'clock-out) "stopped clock")
                     ((eq org-log-note-purpose 'done)  "closed todo item")
                     ((eq org-log-note-purpose 'state)
                      (format "state change from \"%s\" to \"%s\""
                              (or org-log-note-previous-state "")
                              (or org-log-note-state "")))
                     ((eq org-log-note-purpose 'reschedule)
                      "rescheduling")
                     ((eq org-log-note-purpose 'delschedule)
                      "no longer scheduled")
                     ((eq org-log-note-purpose 'redeadline)
                      "changing deadline")
                     ((eq org-log-note-purpose 'deldeadline)
                      "removing deadline")
                     ((eq org-log-note-purpose 'refile)
                      "refiling")
                     ((eq org-log-note-purpose 'note)
                      "this entry")
                     ((eq org-log-note-purpose 'property)
                      (format "\"%s\" property change from \"%s\""
                              (or org-log-note-state "")
                              (or org-log-note-previous-state "")))
                     (t (error "This should not happen")))))
    (when org-log-note-extra (insert org-log-note-extra))
    (setq-local org-finish-function 'org-store-log-note)
    (run-hooks 'org-log-buffer-setup-hook)))

(add-hook 'org-property-changed-functions 'my/org-property-change-note)

(defun my/org-link-copy (&optional arg)
  "Copy the entire org-link (link and description) at point and put it on the killring.
With prefix C-u, just copy the org-link link."
  (interactive "P")
  (when (org-in-regexp org-bracket-link-regexp 1)
    (if (null arg)
        (let ((link (match-string-no-properties 0)))
          (kill-new link)
          (message "Copied link: %s" link))
      (let ((link (org-link-unescape (match-string-no-properties 1))))
        (kill-new link)
        (message "Copied link: %s" link)))))

(define-key org-mode-map "\C-ch" 'my/org-link-copy)

;; Add ability to move forward by timestamp
(setq my/org-timestamp-search-failed nil)

;; Note: Need to advise org-context since exiting the logbook doesn't collapse it.
(defun my/org-next-timestamp (&optional search-backward)
  "Move forward to the next timestamp.
If the timestamp is in hidden text, expose it."
  (interactive "P")
  (when (and my/org-timestamp-search-failed (eq this-command last-command))
    (goto-char (point-min))
    (message "Timestamp search wrapped back to beginning of buffer"))
  (setq my/org-timestamp-search-failed nil)
  (let* ((pos (point))
         (ct (org-context))
         (a (assq :timestamp ct))
         (srch-fun (if search-backward 're-search-backward 're-search-forward)))
    (cond (a (goto-char (nth (if search-backward 1 2) a)))
          ((looking-at org-element--timestamp-regexp)
           ;; Don't stay stuck at timestamp without an org-link face
           (forward-char (if search-backward -1 1))))
    (if (funcall srch-fun org-element--timestamp-regexp nil t)
        (progn
          (goto-char (match-beginning 0))
          (when (org-invisible-p) (org-show-context)))
      (goto-char pos)
      (setq my/org-timestamp-search-failed t)
      (message "No further timestamps found"))))

(define-key org-mode-map "\C-ct" 'my/org-next-timestamp)

(defun my/org-timestamp-convert-dirty-regexp-hack ()
  "Convert all org-mode timestamps in buffer from the
form <2019-01-04 08:00-10:00> to <2019-01-04 08:00>--<2019-01-04 10:00>"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "<\\([0-9- A-z]\\{15\\}\\)\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)-\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)>" nil t)
    (replace-match "<\\1\\2>--<\\1\\3>" t nil)))

;; Have dired store org-link when a file or files(s) are renamed
(defun my/dired-rename-file (file newname ok-if-already-exists)
  "Store org-link to the renamed file."
  (push (list newname (file-name-nondirectory newname)) org-stored-links))

(advice-add #'dired-rename-file :after #'my/dired-rename-file)

;; Org-mode Priorities
;; Default highest is 65 (A)
;; Default default is 66 (B)
;; Default lowest is 65 (C)
;; Default should be "C" - two levels of priority should be enough
(setq org-default-priority 67)

;; Insert Capture Template At End of Current
(defun my/org-capture-after-current ()
  (interactive)
  (org-insert-heading-after-current)
  (kill-line -1)
  (org-return)
  (delete-forward-char 1)
  ;; This doesn't work for some reason
  ;; (let ((current-prefix-arg (digit-argument 0)))
  ;;   (call-interactively #'org-capture)))
  (execute-kbd-macro (read-kbd-macro "C-u 0 C-c c")))

(defun my/org-backup-files ()
  "Create backup of entire org-mode directory in the archive directory. I really need to start
using git for this instead."
  (interactive)
  (let* ((date-time (format-time-string "%Y-%m-%d %H.%M.%S"))
         (org-backup-directory
          (concat (cdr (assoc "val" org-link-abbrev-alist)) "org/Archive/" date-time)))
    (copy-directory org-directory org-backup-directory)
    (message "%s" (concat org-directory " copied to " org-backup-directory "!"))))
