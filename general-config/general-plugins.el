;; Add ~/.emacs.d/plugins to load-path
(add-to-list 'load-path "~/.emacs.d/plugins/")

;; Add all sub-directories to load-path
(let ((default-directory "~/.emacs.d/plugins/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Enable winner-mode
(winner-mode t)

;; Enable nastran-mode
(require 'nastran-mode)
(add-to-list 'auto-mode-alist '("\\.bdf$" . nastran-mode))
(add-to-list 'auto-mode-alist '("\\.dat$" . nastran-mode))
(add-hook 'nastran-mode-hook 'turn-on-font-lock)

;; Enable abaqus-mode
(require 'abaqus-mode)
(add-to-list 'auto-mode-alist '("\\.inp$" . abaqus-mode))
(add-hook 'abaqus-mode-hook 'turn-on-font-lock)

;; Enable htmlize
(require 'htmlize)

;; Enable request
(require 'request)

;; Enable popwin
;; (require 'popwin)
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
