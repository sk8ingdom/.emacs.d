;; Disable toolbar
(tool-bar-mode nil)

;; Set ssl client
;; (setq tls-program '("\"C:/Program Files (x86)/Git/bin/openssl.exe\" s_client -connect %h:%p -no_ssl2 -ign_eof -crlf"))
;; (setq tls-program '("\"C:/Program Files (x86)/Git/bin/openssl.exe\" s_client -connect %h:%p -no_ssl2 -ign_eof"
;;                     "gnutls-cli -p %p %h"))
;;(load "~/.emacs.d/plugins/smtp-openssl.el")
;; (setq gnutls-trust-files '("C:/cygwin/usr/ssl/certs/ca-bundle.trust.crt" "C:/cygwin/usr/ssl/certs/ca-bundle.crt"))

(setq tls-program '("openssl s_client -CApath C:\\cygwin\\usr\\ssl\\certs -connect imap.gmail.com:993"))
(setq gnutls-trustfiles '("C:\\cygwin\\usr\\ssl\\certs\\ca-bundle.crt" "C:\\cygwin\\usr\\ssl\\certs\\ca-bundle.trust.crt" "C:\\cygwin\\usr\\ssl\\certs\\demo\\ca-cert.pem"))

;; Tramp configuration
(require 'tramp)
(set-default 'tramp-auto-save-directory "C:\\Users\\surano\\AppData\\Local\\Temp")
;; Can alternatively be set to pscp
(set-default 'tramp-default-method "plink")
(defun my/comint-init ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'my/comint-init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(tool-bar-mode nil)
 '(menu-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Courier New" :foundry "outline" :slant normal :weight normal :height 83 :width normal)))))

;; Initial frame size and location
(setq initial-frame-alist
      '((top . -555)
        (left . 1600)
        (width . 167)
        (height . 72)))

;; Subsequent frame size and location
;; Call gnus with M-x gnus-other-frame
(setq default-frame-alist
      '((top . -235)
        (left . 1600)
        (width . 166)
        (height . 37)))

;; Map server
(add-hook 'emacs-startup-hook
          (lambda ()
            (call-process-shell-command "U:/dev/Programs/Batch/Scripts/webdav/webdav-connect.bat")))

(add-hook 'kill-emacs-hook
          (lambda ()
            (call-process-shell-command "U:/dev/Programs/Batch/Scripts/webdav/webdav-disconnect.bat")))

;; Gnus
(load "~/.emacs.d/windows-config/windows-gnus.el")
