;;; -*- lexical-binding: t -*-

;; ;; Enable package
(require 'package)
;; Used to overcome bad-signature errors.
(setq package-check-signature nil)
(package-initialize)

;; Only do if connected to the internet
;; (package-refresh-contents)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa"        . "http://www.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;             '("org"          . "http://orgmode.org/elpa/") t)

;; ;; Auto-update packages
(add-to-list 'load-path "~/.emacs.d/plugins/auto-package-update.el")
(require 'auto-package-update)

;; Only do if connecte to the internet
;; (auto-package-update-maybe)

;; Disable the splash screen
(setq inhibit-splash-screen t)

;; Turn off blinking cursor
(blink-cursor-mode -1)

;; Turn off tool-bar
(tool-bar-mode -1)

;; Turn off menu-bar
(menu-bar-mode -1)

;; Enable transient mark mode
(transient-mark-mode t)

;; Set truncate lines to default
(set-default 'truncate-lines t)

;; Font
(set-face-attribute 'default nil
                    :family "Courier New"
                    :foundry "outline"
                    :slant 'normal
                    :weight 'normal
                    :height 83
                    :width 'normal)

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
;; Increase 10X via https://www.reddit.com/r/emacs/comments/819v0h/how_to_speed_up_cursor_movement_by_10x/
(setq auto-window-vscroll nil)

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
(defun my/dos2unix ()
  "Convert a DOS formatted text buffer to UNIX format"
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun my/unix2dos ()
  "Convert a UNIX formatted text buffer to DOS format"
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; From http://www.emacswiki.org/emacs/FindingNonAsciiCharacters
(defun my/find-next-unsafe-char (&optional coding-system)
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

;; Saving

;; Disable git
(setq vc-handled-backends ())

;; Disable backup
(setq backup-inhibited t)

;; ;; Disable auto-save
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

;; Prefer vertical splits (mainly to fix org-agenda)
(setq split-height-threshold nil)

;; Enable narrowing to region (detault is C-x n n)
(put 'narrow-to-region 'disabled nil)

;; Enable upcase-region
(put 'upcase-region 'disabled nil)

;; Reduce and re-colorfringe
(set-fringe-mode '(3 . 3))
(set-face-background 'fringe nil)

;; Replace audio bell with visual bell
(setq visible-bell nil
      ring-bell-function 'my/flash-fringe)

(defun my/flash-fringe ()
  (let ((fringe (face-background 'fringe)))
    (set-face-background 'fringe "gray95")
    (run-with-idle-timer 0.05 nil
                         (lambda ()
                           (set-face-background 'fringe fringe)))))

;; Turn off dialog-boxes
(setq use-dialog-box nil)

;; Start Emacs server
(server-start)
