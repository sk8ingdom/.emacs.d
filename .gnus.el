(setq user-mail-address "sk8ingdom@gmail.com"
      user-full-name "Dominic Surano")

(setq gnus-ignored-newgroups "")

;; (setq gnus-select-method '(nnimap "gmail"
;;                                   (nnimap-address "imap.gmail.com")
;;                                   (nnimap-server-port 993)
;;                                   (nnimap-stream ssl)))

(setq gnus-select-method '(nntp "news.gmane.org"))

(require 'nnreddit)

(setq gnus-secondary-select-methods
;;       '((nntp "news.gmane.org")
;;         (nntp "news.eternal-september.org")
;;         (nntp "news.gwene.org")
      '((nnreddit "")))

;; (gnus-group-unsubscribe-group "nnreddit:emacs")

;; Set parameters
;; (setq gnus-parameters
;;       '(("nnreddit nnreddit:emacs"
;;          (display . all))))

;; (require 'smtpmail)
;; (setq send-mail-function 'smtpmail-send-it
;;       smtpmail-auth-credentials "~/.authinfo"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; Start in topic mode
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Automatically fold when entering a topic
(add-hook 'gnus-summary-prepared-hook 'gnus-summary-hide-all-threads)

;; Eliminate duplicates
(setq gnus-summary-ignore-duplicates t)
(setq gnus-suppress-duplicates t)

;; Fold topics with tab
;; http://www.emacswiki.org/emacs/GnusTopics
(defun gnus-topic-fold-this-topic nil
  "Toggle folding of current topic."
  (interactive)
  (gnus-topic-goto-topic (gnus-current-topic))
  (gnus-topic-fold))

;;(add-hook 'gnus-group-mode-hook '(define-key gnus-topic-mode-map (kbd "<tab>") 'gnus-topic-fold-this-topic))
(with-eval-after-load 'gnus-topic
  (define-key gnus-topic-mode-map (kbd "<tab>") 'gnus-topic-fold-this-topic))

;; Highlight current line in summary mode
(add-hook 'gnus-summary-prepare-hook 'hl-line-mode)
