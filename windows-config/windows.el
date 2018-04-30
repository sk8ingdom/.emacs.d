;; Set ssl client
;; (setq tls-program '("\"C:/Program Files (x86)/Git/bin/openssl.exe\" s_client -connect %h:%p -no_ssl2 -ign_eof -crlf"))
;; (setq tls-program '("\"C:/Program Files (x86)/Git/bin/openssl.exe\" s_client -connect %h:%p -no_ssl2 -ign_eof"
;;                     "gnutls-cli -p %p %h"))
;;(load "~/.emacs.d/plugins/smtp-openssl.el")
;; (setq gnutls-trust-files '("C:/cygwin/usr/ssl/certs/ca-bundle.trust.crt" "C:/cygwin/usr/ssl/certs/ca-bundle.crt"))

;; (setq tls-program '("openssl s_client -CApath C:\\cygwin\\usr\\ssl\\certs -connect imap.gmail.com:993"))
;; (setq gnutls-trustfiles '("C:\\cygwin\\usr\\ssl\\certs\\ca-bundle.crt" "C:\\cygwin\\usr\\ssl\\certs\\ca-bundle.trust.crt" "C:\\cygwin\\usr\\ssl\\certs\\demo\\ca-cert.pem"))
(setq gnutls-trustfiles '("C:\\Users\\dominics\\Documents\\mul\\Programs\\Mozilla\\certificates\\2017-09-20\\cacert.pem"))

;; (setq tls-program '("openssl s_client -CApath U:\\mul\\Programs\\GnuTLS\\certs\\ -connect imap.gmail.com:993"))
;; (setq gnutls-trustfiles '("U:\\mul\\Programs\\GnuTLS\\certs\\cacert.perm" "U:\\mul\\Programs\\GnuTLS\\certs\\cacert.crt"))

;; Add scripts to path
;; (add-to-list 'exec-path "C:\\Users\\dominics\\Documents\\mul\\Programs\\GNU Coreutils\\Windows\\coreutils-5.3.0-bin\\bin")
;; (add-to-list 'exec-path "C:\\Users\\dominics\\Documents\\mul\\Programs\\GNU Coreutils\\Windows\\coreutils-5.3.0-dep\\bin")
;; (add-to-list 'exec-path "C:\\Users\\dominics\\Documents\\dev\\Programs\\batch\\Scripts")
;; (add-to-list 'exec-path "C:\\Users\\dominics\\Documents\\mul\\Programs\\gnuwin32\\diffutils-2.8.7-1-bin\\bin")

;; Dired
(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)
(setq ls-lisp-dirs-first t)

;; Tramp configuration
(require 'tramp)
(set-default 'tramp-auto-save-directory "C:\\Users\\surano\\AppData\\Local\\Temp")
;; Can alternatively be set to pscp
(set-default 'tramp-default-method "plink")
(defun my/comint-init ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'my/comint-init)

;; Initial frame size and location
;; Horizontal on RH monitor
;; (setq initial-frame-alist
;;       '((top . 540)
;;         (left . 1920)
;;         (width . 270)
;;        (height . 36)))
;; Vertical on RH monitor
;; (setq initial-frame-alist
;;      '((top . 0)
;;        (left . 1920)
;;        (width . 133)
;;        (height . 75)))

;; Subsequent frame size and location
;; Call gnus with M-x gnus-other-frame
;; (setq default-frame-alist
;;      '((top . 0)
;;        (left . 2880)
;;        (width . 133)
;;        (height . 75)))
(setq default-frame-alist '((top . 0)
                            ;; Left monitor
                            ;; (left . 0)
                            ;; Middle monitor
                            (left . 1920)
                            ;; Right monitor
                            ;; (left . 3840)
                            (fullscreen . maximized)))

;; Map server
(add-hook 'emacs-startup-hook
          (lambda ()
            (call-process-shell-command "c:/Users/dominics/Documents/dev/Programs/Batch/Scripts/webdav/webdav-connect.bat")))

(add-hook 'kill-emacs-hook
          (lambda ()
            (call-process-shell-command "c:/Users/dominics/Documents/dev/Programs/Batch/Scripts/webdav/webdav-disconnect.bat")))

;; Gnus
(load "~/.emacs.d/windows-config/windows-gnus.el")

;; pdf-tools
(require 'pdf-tools)

;; Install before use; needs conditional
(pdf-tools-install)

;; Audomatically fit page to window
(add-hook 'pdf-tools-enabled-hook 'pdf-view-fit-page-to-window)
;; Make quit work as intended
(define-key pdf-view-mode-map "q" (lambda () (interactive) (kill-buffer (current-buffer))))
;; Replace helm-swoop bindings
(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)

;; ;; message-outlook
;; (setq mail-user-agent 'message-user-agent)
;; (require 'message-outlook)
;; ;; Uses ~/bin/outlook_emacs.wsf
;; (require 'outlookedit)

;; Configuration for olm, Outlook Mail
;; Requires defining:
;;     (defun string-to-int (string &optional base)
;;      (string-to-number string base))
;; or just replacing 'string-to-int with 'string-to number
(require 'olm)
(global-set-key (kbd "C-x m") 'olm)
(add-hook 'olm-summary-mode-hook 'my/make-frame-with-focus)
(add-hook 'olm-summary-mode-quit-hook 'my/delete-frame-previous-focus)
(setq olm-folders-alist '(("References" . "\\\\dominic.surano@insitu.com\\References")))
