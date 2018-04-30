;; Dired

;; Add dot files and directories first
(setq dired-listing-switches "-laGh1v")

;; Change time format
(setq ls-lisp-format-time-list  '("%Y-%m-%d %a %H:%M"))
(setq ls-lisp-use-localized-time-format t)

;; For some reason dired-at-point seems to be broken
;; (global-set-key (kbd "C-x d") 'dired)

;; Allow r and R to rename files
(define-key dired-mode-map (kbd "r") 'dired-do-rename)

;; Allow dired to delete or copy directories
(setq dired-recursive-copies 'always)

;; Allow dired-find-alternate-file without prompt
(put 'dired-find-alternate-file 'disabled nil)

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

;; ;; Advise quit-window to kill buffer instead of bury
;; (defadvice my/quit-window (before quit-window-always-kill)
;;   "When running `quit-window', always kill the buffer."
;;   (ad-set-arg 0 t))

;; (ad-activate 'my/quit-window)

;; Quit seems to also remove the frame, just remind to kill buffer
(define-key dired-mode-map "q" (lambda () (interactive) (kill-buffer (current-buffer))))

;; Was dired-advertised-find-file
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; Disable all mouse activity
;; (define-key dired-mode-map [mouse-1] nil)
;; (define-key dired-mode-map [mouse-2] nil)
;; (define-key dired-mode-map [mouse-3] nil)
;; (define-key dired-mode-map [down-mouse-1] 'dired-find-alternate-file)

;; ;; Disable mouse click on links
;; (defun my/dired-disable-mouse ()
;;   (setq-local mouse-1-click-follows-link nil))

;; (add-hook 'dired-mode-hook 'my/dired-disable-mouse)

(define-key dired-mode-map [mouse-2] 'dired-find-alternate-file)

;; Hide all tooltips
(defun my/dired-disable-tooltips ()
  (setq-local tooltip-delay 100)
  (setq-local tooltip-hide-delay 0))

(add-hook 'dired-mode-hook 'my/dired-disable-tooltips)

;; Was dired-up-directory
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))
(define-key dired-mode-map (kbd "DEL") (lambda () (interactive) (find-alternate-file "..")))
(define-key dired-mode-map (kbd "b") (lambda () (interactive) (find-alternate-file "..")))

(defun my/dired-next-or-previous-line (arg)
" Move forward or backwards between files in dired buffers. \
If arg > 0, move forward. If arg <0, move backwards."
  (interactive)
  ;; If positive, move forward
  (if (> arg 0)
      ;; If not at end of buffer, keep going
      (if (< (line-number-at-pos) (line-number-at-pos (- (point-max) 1)))
          (dired-next-line 1)
        ;; Else, go to beginning of buffer
        (progn
          (goto-char (point-min))
          (dired-next-line 2)))
    ;; Else, move backwards
    ;; If not at beginning of buffer, keep going
    (if (> (line-number-at-pos) 3)
        (dired-previous-line 1)
      ;; Else, go to the end of buffer
      (progn
        (goto-char (point-max))
        (dired-previous-line 1)))))

(define-key dired-mode-map (kbd "<tab>") (lambda () (interactive) (my/dired-next-or-previous-line 1)))
(define-key dired-mode-map (kbd "n") (lambda () (interactive) (my/dired-next-or-previous-line 1)))
(define-key dired-mode-map (kbd "<backtab>") (lambda () (interactive) (my/dired-next-or-previous-line -1)))
(define-key dired-mode-map (kbd "p") (lambda () (interactive) (my/dired-next-or-previous-line -1)))

;; Doc-view mode

;; Doesn't currently work; trouble with M-x toggle-debug-on-error
;; http://emacs.stackexchange.com/questions/7540/doc-view-mode-hook
;; (add-hook 'doc-view-mode-hook 'doc-view-fit-width-to-window)

;; Ediff mode

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Create new frame and focus
(defun my/make-frame-with-focus ()
  (let ((frame (make-frame)))
    (select-frame-set-input-focus frame)))

;; Delte frame and focus on previous
(defun my/delete-frame-previous-focus ()
  (delete-frame)
  (other-frame 0))

(add-hook 'ediff-before-setup-hook 'my/make-frame-with-focus)
(add-hook 'ediff-quit-hook 'delete-frame-previous-focus)

;; Calc mode

;; Disable multiplication having precedence over division
(setq calc-multiplication-has-precedence nil)

;; eshell mode
(global-set-key (kbd "C-x e") 'eshell)

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize "┌─[" 'face `(:foreground "black"))
         (propertize (user-login-name) 'face `(:foreground "red"))
         (propertize "@" 'face `(:foreground "black"))
         (propertize (system-name) 'face `(:foreground "blue"))
         (propertize "]──[" 'face `(:foreground "black"))
         (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "red"))
         (propertize "]──[" 'face `(:foreground "black"))
         (propertize (concat (eshell/pwd)) 'face `(:foreground "blue"))
         (propertize "]\n" 'face `(:foreground "black"))
         (propertize "└─>" 'face `(:foreground "black"))
         (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "black")))))

;; List-packages
(global-set-key (kbd "C-x l") 'list-packages)

;; List-processes
(global-set-key (kbd "C-x p") 'list-processes)

;; Emacs Lisp mode
(define-key emacs-lisp-mode-map (kbd "C-c w") 'comment-region)
(define-key emacs-lisp-mode-map (kbd "C-c u") 'uncomment-region)
