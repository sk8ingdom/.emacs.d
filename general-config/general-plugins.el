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
(add-to-list 'auto-mode-alist '("\\.inp$" . nastran-mode))
(add-hook 'nastran-mode-hook 'turn-on-font-lock)

;; Enable abaqus-mode
(require 'abaqus-mode)
;; (add-to-list 'auto-mode-alist '("\\.inp$" . abaqus-mode))
(add-hook 'abaqus-mode-hook 'turn-on-font-lock)

;; Enable htmlize
(require 'htmlize)

;; Enable request
(require 'request)

;; Enable helm
;; (require 'helm-config)
;; (helm-mode t)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

;; Enable Shakle
(require 'shackle)
(setq shackle-default-rule '(:same t))
(setq shackle-rules
      '(;; Work
        ("\\`\\*helm.*?\\*\\'" :regexp t :align below :ratio 0.20)
        (compilation-mode                :align below :ratio 0.20)
        ("*Buffer List*"                 :align below :ratio 0.33 :select t)
        ("*Calendar*"                    :align below :ratio 10   :select t)
        (" *Deletions*"                  :align below)
        ("*Occur*"                       :align below :ratio 0.20)
        ("*Completions*"                 :align below :ratio 0.20)
        ("*Help*"                        :align below :ratio 0.33 :select t)
        (" *Metahelp*"                   :align below :ratio 0.20 :select t)
        ("*Messages*"                    :align below :ratio 0.20 :select t)
        ("*Warning*"                     :align below :ratio 0.20 :select t)
        ("*Warnings*"                    :align below :ratio 0.20 :select t)
        ("*Backtrace*"                   :align below :ratio 0.20 :select t)
        ("*Compile-Log*"                 :align below :ratio 0.20)
        ("*package update results*"      :align below :ratio 0.20)
        ("*Ediff Control Panel*"         :align below             :select t)

        ;; Org-mode
        (" *Org todo*"                   :align below :ratio 10   :select t)
        ("*Org Note*"                    :align below :ratio 10   :select t)
        ("CAPTURE.*"           :regexp t :align below :ratio 20)
        ("*Org Select*"                  :align below :ratio 20)
        ("*Org Links*"                   :align below :ratio 10)
        (" *Agenda Commands*"            :align below)
        ("*Org Clock*"                   :align below)
        ("*Edit Formulas*"               :align below :ratio 10   :select t)
        ("\\*Org Src.*"        :regexp t :align below :ratio 10   :select t)

        ;; Don't Work
        ;; (dired-mode                      :align below :ratio 0.20 :select t)
        ;; (calc-mode                       :align below :ratio 0.20 :select t)
        ;; ("*Calculator*"                  :align below :ratio 0.33 :select t)
        ;; ("*eshell*"                      :align below :ratio 0.33 :select t)
        ;; (" *RE-Builder*"                 :align below :ratio 0.33 :select t)
        ))
(shackle-mode t)

;; Function to suppress delete-other-windows in functions
(defun my/suppress-delete-other-windows (old-fun &rest args)
  (cl-flet ((silence (&rest args) (ignore)))
    (advice-add 'delete-other-windows :around #'silence)
    (unwind-protect
         (apply old-fun args)
      (advice-remove 'delete-other-windows #'silence))))

;; Adds delete-other-windows to org-mode functions for compatibility with shackle
(advice-add 'org-capture-place-template :around #'my/suppress-delete-other-windows)
(advice-add 'org-agenda :around #'my/suppress-delete-other-windows)
(advice-add 'org-add-log-note :around #'my/suppress-delete-other-windows)

;; Fix org-edit-src-code, called on a source block with C-c ' to work with shackle
(setq org-src-window-setup 'other-window)

;; Enable symon
(require 'symon)
(setq symon-delay 5)
(setq symon-refresh-rate 1)
(symon-mode)
(setq symon-sparkline-type 'plain)
(setq symon-sparkline-height 0)

;; Enable google-this-mode
;; Available on MELPA
;; (google-this-mode t)

;; Enable google-maps-mode
;; Available on MELPA
;; https://julien.danjou.info/projects/emacs-packages#google-maps
(require 'google-maps)

;; Enable dired-quick-sort
;; In dired buffer, activate with S
(require 'dired-quick-sort)
(setq ls-lisp-use-insert-directory-program "ls")
(dired-quick-sort-setup)
