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

;; Enable helm
;; (require 'helm-config)
;; (helm-mode t)
;; (global-set-key (kbd "M-x") 'helm-M-x)

;; Enable Shakle
(require 'shackle)
(setq shackle-default-rule '(:same t))
(setq shackle-rules
      '(
        ;; Works
        ("\\`\\*helm.*?\\*\\'" :regexp t :align below :ratio 0.20)
        (compilation-mode                :align below :ratio 0.20)
        ("*Completions*"                 :align below :ratio 0.20)
        ("*Help*"                        :align below :ratio 0.33 :select t)
        ("*Buffer List*"                 :align below :ratio 0.33 :select t)
        (" *Org todo*"                   :align below :ratio 10   :select t)
        ("*Org Note*"                    :align below :ratio 10   :select t)
        ("*Calendar*"                    :align below :ratio 10   :select t)
        ("CAPTURE.*"           :regexp t :align below :ratio 0.20)
        ("*Org Select*"                  :align below :ratio 0.20)
        ("*Org Links*"                   :align below :ratio 0.20)
        (" *Agenda Commands*"            :align below)

        ;; Doesn't work
        ;; (dired-mode                      :align below :ratio 0.20 :select t)
        ;; (calc-mode                       :align below :ratio 0.20 :select t)
        ;; ("*Calculator*"                  :align below :ratio 0.33 :select t)
        ;; ("*eshell*"                      :align below :ratio 0.33 :select t)
        ;; ("*RE-Builder*"                  :align below :ratio 0.33 :select t)
        ))
(shackle-mode t)

;; Function to suppress delete-other-windows in functions
(defun my/suppress-delete-other-windows (old-fun &rest args)
  (cl-flet ((silence (&rest args) (ignore)))
    (advice-add 'delete-other-windows :around #'silence)
    (unwind-protect
         (apply old-fun args)
      (advice-remove 'delete-other-windows #'silence))))

;; Adds delete-other-windows to org-capture-place-template to compatibility with shackle
(advice-add 'org-capture-place-template :around #'my/suppress-delete-other-windows)
(advice-add 'org-agenda :around #'my/suppress-delete-other-windows)
(advice-add 'org-add-log-note :around #'my/suppress-delete-other-windows)

;; Enable symon
(require 'symon)
(setq symon-delay 5)
(symon-mode)
(setq symon-refresh-rate 1)
(setq symon-sparkline-type 'plain)
(setq symon-sparkline-height 0)
