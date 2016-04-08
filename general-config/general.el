;; Disable the splash screen
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode t)

;; Set truncate lines to default
(set-default 'truncate-lines t)

;; Display time
(display-time-mode t)

;; Scrolling

;; Hides scroll bar
(set-scroll-bar-mode nil)
;; Removes margin
(setq scroll-margin 0)
;; Makes scrolling smoother
(setq scroll-conservatively 10000000)
 ;; One line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; Accelerate scrolling
(setq mouse-wheel-progressive-speed t)
;; Scroll window under mouse
(setq mouse-wheel-follow-mouse 't)
;; Keyboard scroll one line at a time
(setq scroll-step 1)

;; Tabs

;; Disable tabs by default
(setq-default indent-tabs-mode nil)

;; Remove tabs before saving
(add-hook 'write-file-hooks
          (lambda () (if (not indent-tabs-mode)
                         (untabify (point-min) (point-max)))
            nil ))

;; Encoding

;; Set default encoding to us-ascii
(prefer-coding-system       'us-ascii-unix)
(set-default-coding-systems 'us-ascii-unix)
(set-terminal-coding-system 'us-ascii-unix)
(set-keyboard-coding-system 'us-ascii-unix)
(setq default-buffer-file-coding-system 'us-ascii-unix)

;; Switch file encodings
(defun dos2unix ()
  "Convert a DOS formatted text buffer to UNIX format"
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert a UNIX formatted text buffer to DOS format"
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; From http://www.emacswiki.org/emacs/FindingNonAsciiCharacters
(defun find-next-unsafe-char (&optional coding-system)
  "Find the next character in the buffer that cannot be encoded by
coding-system. If coding-system is unspecified, default to the coding
system that would be used to save this buffer. With prefix argument,
prompt the user for a coding system."
  (interactive "Zcoding-system: ")
  (if (stringp coding-system) (setq coding-system (intern coding-system)))
  (if coding-system nil
    (setq coding-system
          (or save-buffer-coding-system buffer-file-coding-system)))
  (let ((found nil) (char nil) (csets nil) (safe nil))
    (setq safe (coding-system-get coding-system 'safe-chars))
    ;; some systems merely specify the charsets as ones they can encode:
    (setq csets (coding-system-get coding-system 'safe-charsets))
    (save-excursion
      ;;(message "zoom to <")
      (let ((end  (point-max))
            (here (point    ))
            (char  nil))
        (while (and (< here end) (not found))
          (setq char (char-after here))
          (if (or (eq safe t)
                  (< char ?\177)
                  (and safe  (aref safe char))
                  (and csets (memq (char-charset char) csets)))
              nil ;; safe char, noop
            (setq found (cons here char)))
          (setq here (1+ here))) ))
    (and found (goto-char (1+ (car found))))
    found))

;; Calc mode

;; Disable multiplication having precedence over division
(setq calc-multiplication-has-precedence nil)

;; Disable git
(setq vc-handled-backends ())

;; Disable backup
(setq backup-inhibited t)

;; Disable auto-save
(setq auto-save-default nil)

;; Automatically convert line endings to unix
;;(add-hook 'find-file-hook 'find-file-check-line-endings)
;;(defun dos-file-endings-p ()
;;      (string-match "dos" (symbol-name buffer-file-coding-system)))
;;(defun find-file-check-line-endings ()
;;      (when (dos-file-endings-p)
;;              (set-buffer-file-coding-system 'undecided-unix)
;;              (set-buffer-modified-p nil)))

;; Enable follow link bindings
(ffap-bindings)

;; Don't ignore hidden matches
(setq search-invisible t)

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Ediff mode
(custom-set-variables
 ;; Puts buffers side by side
 '(ediff-split-window-function (quote split-window-horizontally))
 ;; Added ediff control buffer at bottom; activate with ?
 '(ediff-window-setup-function (quote ediff-setup-windows-plain)))

;; Prefer vertical splits (mainly to fix org-agenda)
(setq split-height-threshold nil)

;; Plugins

;; Enable winner-mode
(winner-mode t)

;; Enable package
;; (require 'package)
;; (package-initialize)
;; (add-to-list 'package-archives
;;             '("melpa" . "https://melpa.org/packages") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Enable nastran-mode
;; (load "~/.emacs.d/plugins/nastran/nastran")
(autoload 'nastran-mode "~/.emacs.d/plugins/nastran-mode/nastran" "Major mode for editing Nastran files." t)
(add-to-list 'auto-mode-alist '("\\.bdf$" . nastran-mode))
(add-to-list 'auto-mode-alist '("\\.dat$" . nastran-mode))
(add-hook 'nastran-mode-hook 'turn-on-font-lock)
;; This is currently required because the top link doesn't currently work in Emacs 24.4
;; (add-hook 'find-file-hooks (lambda () (font-lock-mode t)))

;; Enable abaqus-mode
;; (add-hook 'abaqus-mode-hook 'turn-on-font-lock)
;; (load "~/.emacs.d/plugins/abaqus")
(autoload 'abaqus-mode "~/.emacs.d/plugins/abaqus" "Major mode for editing Abaqus files." t)
(add-to-list 'auto-mode-alist '("\\.inp$" . abaqus-mode))
(add-hook 'abaqus-mode-hook 'turn-on-font-lock)
;; This is currently required because the top link doesn't currently work in Emacs 24.4
;; (add-hook 'find-file-hooks (lambda () (font-lock-mode t)))

;; Enable htmlize
(load "~/.emacs.d/plugins/htmlize")

;; Enable request
(load "~/.emacs.d/plugins/request")

;; Enable web-mode
;; (load "~/.emacs.d/plugins/web-mode")

;; Enable popwin
;; (load "~/.emacs.d/plugins/popwin")
;; (popwin-mode t)
;; (push '("*CAPTURE-ref.org*" :height 0.2 :noselect nil :stick t) popwin:special-display-config)
;; (push '("*Org Agenda*" :height 0.2 :noselect nil :stick t) popwin:special-display-config)
;; (push '("*Org Select*" :height 0.2 :noselect nil :stick t) popwin:special-display-config)
;; (push '("*eshell*" :height 0.2 :noselect nil :stick t) popwin:special-display-config)
;; (push '("^CAPTURE-.+\*.org$" :regexp t) popwin:special-display-config)
;; (push '("Calendar" :height 0.1 :noselect nil :stick t) popwin:special-display-config)
;; (push '(calc-mode :position bottom :height 10) popwin:special-display-config)

;; (push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)
;; (add-hook 'helm-after-initialize-hook (lambda ()
;;                                         (popwin:display-buffer helm-buffer t)
;;                                         (popwin-mode nil)))
;; ;; Fix for HELM?
;; ;; (setq popwin:special-display-config
;; ;;       (append
;; ;;        '(("^\*helm.+\*$" :regexp t)
;; ;;          ("*rspec-compilation*" :height 20)
;; ;;          ("^\*Man .+\*$" :regexp t)
;; ;;          ("*Clock Task Select*" :height 20)
;; ;;          ("^\*Org Agenda.+\*$" :regexp t)
;; ;;          ("*Agenda Commands*")
;; ;;          (org-agenda-mode :position bottom :height 15 :stick t)
;; ;;          ("^CAPTURE-.+$" :regexp t)
;; ;;          ("*Org Select*"))
;; ;;        popwin:special-display-config) )

;; ;;  Restore popwin-mode after a Helm session finishes.
;; (add-hook 'helm-cleanup-hook (lambda () (popwin-mode t)))

;; Enable Shakle
;; (require 'shackle)
;; (setq shackle-default-rule '(:same t))
;; (setq shackle-rules
;;       '(
;;         ;; Works
;;         ("\\`\\*helm.*?\\*\\'" :regexp t :align below :ratio 0.20)
;;         ;; Works
;;         (compilation-mode                :align below :ratio 0.20)
;;         ;; Doesn't work
;;         (dired-mode                      :align below :ratio 0.20 :select t)
;;         ;; Works
;;         ("*Completions*"                 :align below :ratio 0.20)
;;         ;; Works
;;         ("*RE-Builder*"                  :align below :ratio 0.33 :select t)
;;         ;; Works
;;         ("*Help*"                        :align below :ratio 0.33 :select t)
;;         ;; Works
;;         ("*Buffer List*"                 :align below :ratio 0.33 :select t)
;;         ;; Doesn't work
;;         ("*Calculator*"                  :align below :ratio 0.33 :select t)
;;         ;; Doesn't work
;;         ("*eshell*"                      :align below :ratio 0.33 :select t)
;;         ;; Doesn't work
;;         ("*Org Select*"                  :align below :ratio 0.20)
;;         ;; Doesn't work
;;         ("\*CAPTURE.*"         :regexp t :align below :ratio 0.20)))
;; (shackle-mode t)

;; Start Emacs server
(server-start)
