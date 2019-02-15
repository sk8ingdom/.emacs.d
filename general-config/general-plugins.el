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

;; ;; Enable helm
;; (require 'helm-config)
;; (helm-mode t)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; (define-key helm-map (kbd "M-x") 'helm-select-action)
;; (global-set-key (kbd "C-c q") 'helm-mini)

;; ;; Replace list-buffers
;; (global-set-key (kbd "C-x b") 'helm-buffers-list)

;; ;; Replace list-buffers
;; (global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;; ;; Replace find-file-at point
;; ;; https://github.com/emacs-helm/helm/issues/984
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)

;; (recentf-mode t)
;; (setq helm-ff-file-name-history-use-recentf t)
;; ;; (helm-source-buffers-list helm-source-recentf helm-source-buffer-not-found)

;; ;; Modifying helm-find-files-1 to the following didn't work.
;; ;; (helm :sources '(helm-source-find-files helm-source-recentf) ...

;; ;; Enable completion by default
;; (setq helm-ff-auto-update-initial-value t)

;; ;; (setq helm-display-function
;; ;;       'helm-display-buffer-in-own-frame
;; ;;       helm-display-buffer-reuse-frame t
;; ;;       helm-use-undecorated-frame-option t)

;; ;; Color map
;; (eval-after-load 'helm
;;   (lambda ()
;;     (set-face-attribute 'helm-selection nil
;;                         :background "lightgoldenrod2")
;;     (set-face-attribute 'helm-source-header nil
;;                         :background "gray75"
;;                         :family "Courier New"
;;                         :foundry "outline"
;;                         :slant 'normal
;;                         :weight 'bold
;;                         :height 83
;;                         :width 'normal
;;                         :box 1)
;;     (set-face-attribute 'helm-header nil
;;                         ;; Inherits header-line
;;                         :background "white")
;;     (set-face-attribute 'helm-candidate-number nil
;;                         :background "white")
;;     (set-face-attribute 'helm-ff-dotted-directory nil
;;                         :background "white")
;;     (set-face-attribute 'helm-ff-directory nil
;;                         :background "white")
;;     (set-face-attribute 'helm-ff-executable nil
;;                         :foreground nil
;;                         :inherit 'font-lock-comment-face)
;;     (set-face-attribute 'helm-buffer-directory nil
;;                         :background "white")
;;     (set-face-attribute 'helm-visible-mark nil
;;                         :background "gray75")))

;; ;; Eshell history
;; (require 'helm-eshell)

;; (eval-after-load 'eshell-mode
;;           #'(lambda ()
;;               (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

;; ;; Minibuffer history
;; (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; ;; Doesn't currently work
;; ;;;; Enable helm-swoop
;; ;; (require 'helm-swoop)

;; (eval-after-load 'helm-swoop
;;   (lambda ()
;;
;;     ;; Color Map
;;     (set-face-attribute 'helm-swoop-target-line-face nil
;;                         :background "lightgoldenrod2")
;;     (set-face-attribute 'helm-swoop-target-word-face nil
;;                         :background "lightgoldenrod2"
;;                         ;; :foreground "violetred4")))
;;                         :foreground "#b00000")
;;     (global-set-key (kbd "C-s") 'helm-swoop)
;;     (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
;;     (define-key helm-swoop-map (kbd "C-S-s") 'helm-previous-line)

;;     ;; Disable initial input
;;     ;; https://github.com/ShingoFukuyama/helm-swoop/issues/25
;;     (setq helm-swoop-pre-input-function (lambda () nil))
;;     ;; ;; Trim unwanted characters
;;     ;; (setq helm-swoop-pre-input-function
;;     ;;       (lambda ()
;;     ;;         (let* ((text (thing-at-point 'symbol))
;;     ;;                (input (if text (format "%s" (read text)) "")))
;;     ;;           (setq input (replace-regexp-in-string "^[^a-zA-Z0-9]+" "" input))
;;     ;;           (setq input (replace-regexp-in-string "[^a-zA-Z0-9]+$" "" input))
;;     ;;           input)))

;;     ;; Swoop window isn't properly managed by shackle/popwin
;;     ;; https://github.com/ShingoFukuyama/helm-swoop/issues/62
;;     ;; Set to nil initially per http://ergoemacs.org/emacs/elisp_defvar_problem.html
;;     (defvar helm-swoop-split-window-function nil)
;;     (setq helm-swoop-split-window-function
;;           (lambda ($buf)
;;             (display-buffer $buf)))))

;; ;; Disable swoop in certain modes
;; (eval-after-load 'dired
;;   (lambda ()
;;     (define-key dired-mode-map (kbd "C-s") 'isearch-forward)))
;; ;; (define-key dired-mode-map (kbd "C-s") 'isearch-forward)

;; (eval-after-load 'org
;;   (lambda ()
;;     (define-key org-agenda-mode-map (kbd "C-s") 'isearch-forward)))
;; ;; (define-key org-agenda-mode-map (kbd "C-s") 'isearch-forward)

;; ;; This doesn't seem to work, the helm buffer still comes up
;; ;; ;; Org-mode preferences
;; ;; (eval-after-load 'org-mode
;; ;;   (lambda ()
;; ;;     ;; Helm defaults to helm-org-run-insert-link-to-heading-at-marker instead
;; ;;
;; ;;     (define-key org-mode-map (kbd "C-c C-l") 'org-insert-link)))

;; ;; Enable helm-org-rifle
;; (require 'helm-org-rifle)
;; (setq helm-org-rifle-show-path t)
;; (add-hook 'helm-org-rifle-after-command-hook 'org-reveal)

;; (global-set-key (kbd "C-c b") 'helm-org-rifle)

;; ;; Fix for dired-do-rename
;; (defun my/helm-ignore-error (orig-fun &rest args)
;;   "Ignore error caused from exiting `dired-do-rename' command
;; with `helm-mode' enabled"
;;   (condition-case nil
;;       (apply orig-fun args)
;;     (error nil)
;;     (keyboard-quit)))

;; (advice-add 'expand-file-name :around #'my/helm-ignore-error)

;; Enable undo-tree-mode
(require 'undo-tree)
(global-undo-tree-mode t)

;; Enable Shakle
(require 'shackle)
(setq shackle-default-rule nil)
(setq shackle-rules
      '(;; Helm
        ("\\`\\*helm.*?\\*\\'"    :regexp t :align below :ratio 0.20)
        ("*Helm Swoop*"                     :align below :ratio 0.20)

        ;; Built-in
        (compilation-mode                   :align below :ratio 0.20)
        ("*Buffer List*"                    :align below :ratio 0.33  :select t)
        ("*Calendar*"                       :align below :ratio 10    :select t)
        (" *Deletions*"                     :align below)
        ("*Occur*"                          :align below :ratio 0.20)
        ("*Completions*"                    :align below :ratio 0.20)
        ("*Help*"                           :align below :ratio 0.33  :select t)
        (" *Metahelp*"                      :align below :ratio 0.20  :select t)
        ("*Messages*"                       :align below :ratio 0.20  :select t)
        ("*Warning*"                        :align below :ratio 0.20  :select t)
        ("*Warnings*"                       :align below :ratio 0.20  :select t)
        ("*Backtrace*"                      :align below :ratio 0.20  :select t)
        ("*Compile-Log*"                    :align below :ratio 0.20)
        ("*package update results*"         :align below :ratio 0.20)
        ("*Ediff Control Panel*"            :align below              :select t)
        ("*tex-shell*"                      :align below :ratio 0.20  :select t)
        ("*Dired Log*"                      :align below :ratio 0.20  :select t)
        ;; ("*Marked Files*"                   :align below              :select t)
        (" *Choices*"                       :align below              :select t)
        ("*Register Preview*"               :align below              :select t)
        ("*Finder*"                         :align below :ratio 0.33  :select t)
        ("*Process List*"                   :align below :ratio 0.20  :select t)

        ;; Magit
        ("*magit-commit-popup*"             :align below              :select t)
        ("*magit-dispatch-popup*"           :align below              :select t)

        ;; Magithub
        ("*magithub-dispatch-popup*"       :align below              :select t)

        ;; Plugins
        ;; ("\\`\\*Helpful.*?*\\'"   :regexp t :align below :ratio 0.33  :select t)
        (" *undo-tree*"                     :align right :ratio 0.10  :select t)
        (" *command-log*"                   :align right :ratio 0.20)

        ;; Org-mode
        (" *Org todo*"                      :align below :ratio 10    :select t)
        ("*Org Note*"                       :align below :ratio 10    :select t)
        ("CAPTURE.*"              :regexp t :align below :ratio 20)
        ("*Org Select*"                     :align below :ratio 20)
        ("*Org Links*"                      :align below :ratio 10)
        (" *Agenda Commands*"               :align below)
        ("*Org Clock*"                      :align below)
        ("*Edit Formulas*"                  :align below :ratio 10    :select t)
        ("\\*Org Src.*"           :regexp t :align below :ratio 10    :select t)
        ("*Org Attach*"                     :align below              :select t)
        ("*Org Export Dispatcher*"          :align below              :select t)
        ("*Select Link*"                    :align below              :select t)

        ;; PDF Tools
        ("*PDF-Occur*"                      :align below :ratio 0.20  :select t)
        ("\\*Edit Annotation.*\\*":regexp t :align below :ratio 0.10  :select t)
        ("*Contents*"                       :align below :ratio 0.10)
        ("\\*.* annots\\*"        :regexp t :align below :ratio 0.20  :select t)

        ;; Don't Work
        ;; (dired-mode                         :align below :ratio 0.20  :select t)
        ;; (calc-mode                          :align below              :select t)
        ;; (" *Calculator*"                    :align below              :select t)
        ;; ("*Calc Trail"                      :align below              :select t)
        ;; ("*eshell*"                         :align below :ratio 0.33  :select t)
        ;; (" *RE-Builder*"                    :align below :ratio 0.33  :select t)
        ;; ("*Packages*"                       :align below :ratio 0.33  :select t)
        ;; ("*info*"                           :align below :ratio 0.20  :select t)
        ;; ("\\`\\*Customize.*?*\\'" :regexp t :align below :ratio 0.33  :select t)
        ))
(shackle-mode t)

;; Function to suppress delete-other-windows in functions
(defun my/suppress-delete-other-windows (old-fun &rest args)
  (cl-flet ((silence (&rest args) (ignore)))
    (advice-add 'delete-other-windows :around #'silence)
    (unwind-protect
         (apply old-fun args)
      (advice-remove 'delete-other-windows #'silence))))

;; Adds my/suppress-delete-other-windows to org-mode functions for compatibility with shackle
(advice-add 'org-capture-place-template :around #'my/suppress-delete-other-windows)
(advice-add 'org-agenda :around #'my/suppress-delete-other-windows)
(advice-add 'org-add-log-note :around #'my/suppress-delete-other-windows)
(advice-add 'org-add-log-note :around #'my/suppress-delete-other-windows)

;; Fix org-edit-src-code, called on a source block with C-c ' to work with shackle
(setq org-src-window-setup 'other-window)

;; ;; Enable symon
;; (require 'symon)
;; (setq symon-delay 5)
;; (setq symon-refresh-rate 1)
;; (symon-mode)
;; (setq symon-sparkline-type 'plain)
;; (setq symon-sparkline-height 0)

;; Enable google-this-mode
;; Available on MELPA
;; (google-this-mode t)

;; ;; Enable google-maps-mode
;; ;; Available on MELPA
;; ;; https://julien.danjou.info/projects/emacs-packages#google-maps
;; (require 'google-maps)

;; ;; Enable dired-quick-sort
;; In dired buffer, activate with S
;; (require 'dired-quick-sort)
;; (setq ls-lisp-use-insert-directory-program "ls")
;; (dired-quick-sort-setup)

;; Enable dired-column-widths
;; Didn't work as intended
;; (require 'dired-column-widths)

;; Enable elmacro
(require 'elmacro)
(elmacro-mode)

;; ;; Enable dedicated-mode
;; (require 'dedicated)
;; (require 'dedicate-windows-manually)
;;
;;(global-set-key "\C-ck" 'dedicated-mode)

;; ;; Enable helpful-mode
;; (require 'helpful)

;; (global-set-key (kbd "C-h f") #'helpful-callable)
;; (global-set-key (kbd "C-h v") #'helpful-variable)
;; (global-set-key (kbd "C-h k") #'helpful-key)
;; (global-set-key (kbd "C-h p") #'helpful-at-point)
;; (define-key helpful-mode-map "q" (lambda ()
;;                                    (interactive)
;;                                    (quit-window t)))

;; (define-key help-mode-map (kbd "q") (lambda ()
;;                                       (interactive)
;;                                       (quit-window t)))

;; ;; Enable purpose
;; (require 'window-purpose)
;; (purpose-mode)
;; (setq purpose-use-default-configuration nil)
;; (add-to-list 'purpose-user-mode-purposes '(dired-mode . other))
;; (add-to-list 'purpose-user-name-purposes '("*Org Agenda*" . other))
;; (purpose-compile-user-configuration)

;; Enable ace-window
(require 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

;; Ace configuration for help
(require 'ace-link)
(ace-link-setup-default)

;; ;; Ace configuration for helpful
;; (defun my/help-find-file-ace-window ()
;;   "Use ace window to select a window for opening a file from dired."
;;   (interactive)
;;   (let* ((button (button-at (point)))
;;          (file (button-get button 'path)))
;;     ;; TODO: Add condition to open non-url links normally.
;;     (if (null file)
;;         (helpful-visit-reference)
;;       ;; TODO: Consider adding prefix to quit help window when following link.
;;       (if (> (length (aw-window-list)) 1)
;;           (aw-select "" (lambda (window)
;;                           (aw-switch-to-window window)
;;                           (find-file file)))
;;         (find-file-other-window file)))))

;; (define-key helpful-mode-map "o" 'my/help-find-file-ace-window)
;; (define-key helpful-mode-map (kbd "<return>") 'my/help-find-file-ace-window)

;; Ace advice for help
(defun my/help-button-action-advice (orig-fun &rest args)
  "Use `ace-window' to choose window to open help link. To find
text properties of buttons, use `text-properties-at'."
  ;; If button doesn't have a file path property, treat normally.
  (if (null (cadr (button-get button 'help-args)))
      (apply orig-fun args)
    ;; If button has file path, prompt which window to use.
    (let ((help-window (get-buffer-window)))
      (setq shackle-default-rule '(:same t))
      (aw-select "" (lambda (window)
                      (aw-switch-to-window window)
                      (apply orig-fun args)
                      (quit-window t help-window)
                      ;; This shouldn't have to be called again, but function doesn't work without it
                      (select-window window)))
      (setq shackle-default-rule nil))))

(advice-add 'help-button-action :around #'my/help-button-action-advice)
;; (advice-remove 'help-button-action 'my/help-button-action-advice)

;; Ace advice for dired
(defun my/dired-find-file-advice (orig-fun &rest args)
  "Use `ace-window' to choose window to open file."
  (message prefix-arg)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (apply orig-fun args)
      (aw-select "" (lambda (window)
                      (aw-switch-to-window window)
                      (find-file file))))))

(advice-add 'dired-find-file :around #'my/dired-find-file-advice)
(advice-add 'dired-find-file-other-window :around #'my/dired-find-file-advice)
(advice-add 'dired-find-alternate-file :around #'my/dired-find-file-advice)
(advice-add 'dired-mouse-find-file-other-window :around #'my/dired-find-file-advice)
(advice-add 'dired-display-file :around #'my/dired-find-file-advice)

;; Ace advice for org-open-at-point
;; (defun my/org-open-at-point (orig-fun &rest args)

;; ;; Ace-isearch
;; (require 'ace-isearch)
;; (ace-isearch-mode t)
;; (global-ace-isearch-mode t)

;; ;; Enable icicles
;; (require 'icicles)
;; (global-set-key (kbd "C-x o") 'ace-window)

;; ;; Enable command-log
;; Need to fix clm/open-command-log-buffer which splits frame
;; (defun my/command-log-mode-toggle ()
;;   (interactive)
;;   (require 'command-log-mode)
;;   (if (> my/command-log-mode-on 0)
;;       (progn
;;         (require 'command-log-mode)
;;         (global-command-log-mode 'my-command-log-mode-on)
;;         (clm/close-command-log-buffer)
;;         (setq my/command-log-mode-on -1))
;;     (progn
;;       (global-command-log-mode)
;;       (add-to-list 'shackle-rules '("*command-log*"                    :align below :ratio 3))
;;       (clm/open-command-log-buffer)
;;       (setq my/command-log-mode-on 1))))

;; Which-key-mode configuration
(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 1.0)

;; ;; Magit mode
;; ;; This isn't currently working for some reason.
;; ;; (require 'magit)

;; (global-set-key (kbd "C-x g") 'magit-status)

;; ;; https://magit.vc/manual/magit/Switching-Buffers.html
;; ;; (setq magit-display-buffer-function

;; ;; (define-key magit-mode-map (kbd "q") 'delete-frame)

;; ;; Magithub
;; ;; https://github.com/vermiculus/magithub/issues/299
;; (use-package magithub
;;   :demand t
;;   :after magit
;;   :init
;;   ;; fixme this is a temporary hack; see https://github.com/vermiculus/magithub/issues/299
;;   (define-error 'ghub-404 "Not Found" 'ghub-http-error)
;;   :config
;;   (magithub-feature-autoinject t))

;; Ivy mode
(ivy-mode t)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-wrap t)
(setq ivy-height 10)
(setq ivy-fixed-height-minibuffer t)
;; (setq ivy-count-format "%d/%d")
(setq ivy-count-format "")
(setq ivy-count-format "%d/%d ")
(setq ivy-format-function 'ivy-format-function-arrow)

;; Keybindings
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; Mark Solution
(defvar ivy-marked-candidates '()
  "Holds marked candidates")

(defun ivy-mark-candidate ()
  "Add current candidate to `ivy-marked-candidates'.
If candidate is already in, remove it."
  (interactive)
  (let ((cand (or (assoc ivy--current (ivy-state-collection ivy-last))
               ivy--current)))
    (if (-contains? ivy-marked-candidates cand)
     ;; remove it from the marked list
     (setq ivy-marked-candidates
           (-remove-item cand ivy-marked-candidates))

      ;; add to list
      (setq ivy-marked-candidates
         (append ivy-marked-candidates (list cand)))))
  (ivy-next-line))

(defun ivy-show-marked-candidates ()
  "Show marked candidates."
  (interactive)
  (when ivy-marked-candidates
    (setf (ivy-state-collection ivy-last) ivy-marked-candidates)
    (setf (ivy-state-preselect ivy-last) ivy--current)
    (ivy--reset-state ivy-last)))

(define-key ivy-minibuffer-map (kbd "C-<SPC>") 'ivy-mark-candidate)

;; Show keystrokes
(require 'mwe-log-commands)
(defun my/log-commands-print-strokes ()
  (interactive)
  (mwe:log-keyboard-commands)
  (mwe:open-command-log-buffer))

(global-set-key (kbd "C-x f") 'my/log-commands-print-strokes)

;; Enable feebleline
;; (require 'feebleline)
;; (feebleline-default-settings)

;; Enable minibuffer-line-mode
(minibuffer-line-mode t)
(setq-default mode-line-format nil)
(setq minibuffer-line-refresh-interval 10)

;; This doesn't currently work as intended
(setq minibuffer-line-format '("" (:eval (format-time-string "%F %I:%M %p")) " | %b | %p"))
(add-hook 'window-configuration-change-hook 'my/mode-line-update)
;; This is horseshit and doesn't work; the hook isn't even in the file.
;; (add-hook 'ace-window-display-mode-hook 'my/mode-line-update)
(advice-add #'ace-window :after #'my/mode-line-update)
(add-hook 'mouse-leave-buffer-hook 'my/mode-line-update)

(defun my/mode-line-update (&optional window)
  ;; Update the mode-line. Requires argument window in order for ace-window advice to work
  (interactive)
  (minibuffer-line--update))

(set-face-attribute 'mode-line nil
                    :box nil
                    :background "white"
                    :foreground "black")
(set-face-attribute 'mode-line-inactive nil
                    :box nil
                    :background "white"
                    :foreground "black")

;; Minibuffer-line-mode seems to remove window dividers
(when (boundp (window-divider-mode t))
              (setq window-divider-default-bottom-width 1)
              (setq window-divider-default-right-width 1)
              (setq window-divider-default-places t)
              (set-face-attribute 'window-divider nil
                                  ;; Drawn if width is less than 3px
                                  :foreground "black"
                                  :background "black")
              (window-divider-mode))

;; Mode for Reddit
;; (require 'md4rd)
;; (md4rd)
