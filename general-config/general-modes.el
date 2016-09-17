;; Dired

;; Open file(s) in external application
(defun my/dired-open-in-external-application (&optional file)
  "Open the current file or dired marked files in external application(s). The application is chosen from the OS's preference."
  (interactive)
  (let (doIt
        (myFileList
         (cond
          ((string-equal major-mode "dired-mode") (dired-get-marked-files))
          ((not file) (list (buffer-file-name)))
          (file (list file)))))

    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ")))

    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) myFileList))
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)))  myFileList))
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) myFileList))))))

;; Copy full file path and filename to the kill ring
(defun my/dired-copy-path-and-filename-as-kill ()
  "Push the path and filename of the file under point to the kill ring."
  (interactive)
  (message "added %s to kill ring" (kill-new (dired-get-filename))))
