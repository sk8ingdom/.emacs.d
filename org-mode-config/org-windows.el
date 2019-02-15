;; Capture
(setq org-directory "C:/Users/dominics/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/ref.org"))

;; Targets that contribute to the agenda view
(setq org-agenda-files (list org-directory))

;; Abbreviations
(add-to-list 'org-link-abbrev-alist '("local"          . "C:/Users/dominics/Documents/"))
;; (add-to-list 'org-link-abbrev-alist '("val"            . "https://dominicsurano.com:8081/homes/sk8ingdom/"))
(add-to-list 'org-link-abbrev-alist '("val"            . "Z:/"))
;; (add-to-list 'org-link-abbrev-alist '("val"            . "/cygssh:admin@dominicsurano.com:/share/MD0_DATA/homes/sk8ingdom/"))
(add-to-list 'org-link-abbrev-alist '("doors"          . "shell:\"C:/Program Files (x86)/IBM/Rational/DOORS/9.5/bin/protocolhandler.exe\" -r 240 -s 60 -url \"doors:%s\""))
(add-to-list 'org-link-abbrev-alist '("outlook"        . "shell:\"C:/Program Files (x86)/Microsoft Office/Office14/OUTLOOK.EXE\" /select Outlook:"))
(add-to-list 'org-link-abbrev-alist '("outlook-folder" . "shell:\"C:/Program Files (x86)/Microsoft Office/Office14/OUTLOOK.EXE\" /select \"Outlook:\\\\Dominic.Surano@insitu.com\\%s\""))
(add-to-list 'org-link-abbrev-alist '("ie"             . "shell:start \"\" \"C:/Program Files/Internet Explorer/iexplore.exe\" \"%s\""))

(setq org-confirm-shell-link-function nil)

;; Auto-save org files
(add-hook 'org-mode-hook '(lambda ()
                            (interactive)
                            (setq-local auto-save-default t)))

;; Plugins

;; ;; Enable org-outlook
;; (require 'org-outlook)

;; Enable org-screentshot
(require 'org-windows-screenshot)
(setq org-windows-screenshot-command
      "~/.emacs.d/plugins/org-windows-screenshot/screenCapture.bat")
(setq org-windows-screenshot-directory "c:/Users/dominics/Documents/mul/Images/Screenshots/")
(global-set-key "\C-cs" 'org-windows-screenshot)

;; Default programs
;; https://dontomp.wordpress.com/2015/01/31/in-org-mode-have-the-default-program-openoffice-word-or-whatever-open-docx-file-links/
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . default)
        ("\\.docx\\'" . default)
        ("\\.doc\\'" . default)))

;; Windows layout
(defun my/org-windows-layout ()
  (interactive)
  (split-window-below nil)
  (other-window 1)
  (split-window-below nil)
  (delete-window)
  (split-window-right nil)
  (other-window 1)
  (split-window-below nil)
  (other-window 1)
  ;; (find-file (concat org-directory "/wrk.org"))
  (find-file (concat org-directory "/*.org") t)
  (switch-to-buffer "jnl.org" nil 'force-same-window)
  (other-window 2)
  (switch-to-buffer "wrk.org" nil 'force-same-window)
  (other-window 3)
  (dired-at-point org-directory)
  ;; (dired-at-point "c:/Users/dominics/Documents/")
  (dired-at-point user-emacs-directory)
  (kill-buffer "*Messages*")
  (other-window 2)
  (switch-to-buffer "wrk.org" nil 'force-same-window)
  ;; (execute-kbd-macro (kbd "C-c a < a")
  (org-agenda nil "a" "<"))

(my/org-windows-layout)

;; Insert Windows links
(defun my/yank-convert-forward-slashes ()
  "Escape doublequotes in car of kill-ring. Use with S-<MOUSE-3> to copy entire path."
  (interactive)
  (with-temp-buffer
    (yank)
    (goto-char (point-min))
    (while (search-forward "\\" nil t 1)
      (replace-match "/"))
    (goto-char (point-min))
    (while (search-forward "\"" nil t 1)
      (replace-match ""))
    ;; TODO: Remove %20 from JUST the file-name-nondirectory
    ;; (while (search-forward "%20" nil t 1)
    ;;  (replace-match " "))
    (kill-new (buffer-substring-no-properties (point-min) (point-max)))))

(defun my/org-insert-windows-link ()
  (interactive)
  (my/yank-convert-forward-slashes)
  (let ((dir (car kill-ring)))
    (insert "[[" dir "][" (file-name-nondirectory dir) "]]")))

(define-key org-mode-map "\C-ce" 'my/org-insert-windows-link)
