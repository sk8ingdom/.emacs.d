;; Disable the splash screen
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode t)

;; Set truncate lines to default
(set-default 'truncate-lines t)

;; Tabs

;; Disable tabs by default
(setq-default indent-tabs-mode nil)

;; Remove tabs before saving
(add-hook 'write-file-hooks
          (lambda () (if (not indent-tabs-mode)
                         (untabify (point-min) (point-max)))
            nil ))

;; Encoding

;; Set encoding to us-ascii
(prefer-coding-system       'us-ascii-unix)
(set-default-coding-systems 'us-ascii-unix)
(set-terminal-coding-system 'us-ascii-unix)
(set-keyboard-coding-system 'us-ascii-unix)
(setq default-buffer-file-coding-system 'us-ascii-unix)

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

;; Plugins

;; Enable winner-mode
(winner-mode t)

;; Enable package
(require 'package)
(package-initialize)

;; Enable nastran-mode
;; (load "~/.emacs.d/plugins/nastran")

;; Enable abaqus-mode
;; (add-hook 'abaqus-mode-hook 'turn-on-font-lock)
;; (load "~/.emacs.d/plugins/abaqus")
(autoload 'abaqus-mode "~/.emacs.d/plugins/abaqus" "Major mode for editing Abaqus files." t)
(add-to-list 'auto-mode-alist '("\\.inp$" . abaqus-mode))
(add-hook 'abaqus-mode-hook 'turn-on-font-lock)
;; This is currently required because the top link doesn't currently work in Emacs 24.4
(add-hook 'find-file-hooks (lambda () (font-lock-mode 1)))

;; Enable htmlize
(load "~/.emacs.d/plugins/htmlize")

;; Enable request
(load "~/.emacs.d/plugins/request")

;; Enable web-mode
;; (load "~/.emacs.d/plugins/web-mode")

;; Start Emacs server
(server-start)
