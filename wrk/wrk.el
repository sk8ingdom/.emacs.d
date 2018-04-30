(defun my/multi-level-BOM-to-org ()
  (interactive)
  (beginning-of-buffer)
  (while (search-forward "" nil t)
    (replace-match "" nil nil))
  (beginning-of-buffer)
  (while (re-search-forward "^\\(,\\)\\{1,\\}" nil t)
    (replace-match "" nil nil))
  (beginning-of-buffer)
  (while (re-search-forward "^\\([0-9]\\)\\(,\\)\\{1,\\}" nil t)
    (replace-match "\\1 " nil nil))
  (beginning-of-buffer)
  (while (re-search-forward "^\\([0-9]\\)" nil t)
    (replace-match (make-string (+ 1 (string-to-number (match-string 1))) ?*)))
  (org-mode)
  (org-shifttab 100)
  (my/org-sort-buffer)
  (org-up-heading-all 100))

(defun my/org-sort-buffer ()
  "Sort all entries in the current buffer, recursively."
  (interactive)
  (org-map-entries (lambda ()
                     (condition-case x
                         (org-sort-entries nil ?a)
                       (user-error)))))

;; Function doesn't work because the /autorun command switch has been deprecated since 2003
(defun my/weekly-status ()
  (interactive)
  (my/org-export-region-html)
  (call-process-shell-command "\"C:/Program Files (x86)/Microsoft Office/Office14/OUTLOOK.EXE\" /autorun WeeklyStatus"))

(defun my/org-refile-targets-toggle ()
  "Function to toggle between refiling to ALL org-files or
just wrk.org to speed things up.

Still to do:
- [ ] Have it use a helm window to prompt for file.
- [ ] Have it select the file of the task selected in
      org-agenda."

  (interactive)
  (if (equal org-refile-targets '((org-agenda-files :maxlevel . 9)))
      (setq org-refile-targets '(("c:/users/dominics/Documents/org/wrk.org" :maxlevel . 9)))
    (setq org-refile-targets '((org-agenda-files :maxlevel . 9)))))
