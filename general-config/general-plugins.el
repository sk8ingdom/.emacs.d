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
(require 'popwin)
(popwin-mode t)

(setq popwin:popwin:popup-window-height 20)

(setq popwin:special-display-config
      (append
       '(;; Works
         ("*Help*"                        :stick t)
         ("*Completions*"                          :noselect t)
         ("*Occur*"                       :stick t)
         ("*Buffer List*")
         ("*Messages*")
         ;; ("*eshell*"                      :stick t)
         ("*Warnings*")
         (calendar-mode)
         ;; Don't work
         ;; ("^\*Org Agenda.+\*$"  :regexp t)
         ;; ("*Agenda Commands*")
         ;; (org-agenda-mode                 :stick t)
         ;; ("^CAPTURE-.+$"        :regexp t)
         ;; ("*Org Select*")
         (".*Org todo.*"        :regexp t)
         (".*Org Note.*"        :regexp t)
         ;; ("^\*helm.+\*$"        :regexp t)
         ;; (calc-mode)
         ;; (dired-mode)
         ;; (regexp-builder))
         ("*Deletions*"                             :noselect t)
         )
       popwin:special-display-config))

;;  Restore popwin-mode after a Helm session finishes.
(add-hook 'helm-after-initialize-hook (lambda ()
                                        (popwin:display-buffer helm-buffer t)
                                        (popwin-mode nil)))
(add-hook 'helm-cleanup-hook (lambda () (popwin-mode t)))

;; Fix org-todo for use with popwin

;; Macro which creates advice 'template'
(defmacro my/with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
               adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove ,(car adform) ,(nth 2 adform)))
                 adlist))))

;;Function which replaces org-switch-to-buffer-other-window with emacs' original switch-to-buffer-other-window
(defun my/org-todo-same-window (orig-fn)
  "Advice to fix window placement in `org-fast-todo-selection'."
  (let ((override
      '("\\*Org todo\\*|\\*Org Note\\*"
        (display-buffer-use-some-window)
        (inhibit-same-window . nil)))) ;locally sets variable "override" as key-value pair for display-buffer-alist entry
    (add-to-list 'display-buffer-alist override) ;adds the contents of the above defined variable to display-buffer-alist
    (my/with-advice
        ((#'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window))
      (unwind-protect (funcall orig-fn)
        (setq display-buffer-alist
              (delete override display-buffer-alist))))))

;; Injecting the relevant advice into the org-fast-todo-selection function
(advice-add #'org-fast-todo-selection :around #'my/org-todo-same-window)

;; This sort of works as intended
;; (advice-add #'org-add-log-note :around #'my/org-todo-same-window)

;; To fix capture templates--doesn't currently work
;; (advice-add #'org-mks :around #'my/org-todo-same-window)

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

;; Enable symon
(require 'symon)
(symon-mode)
(setq symon-refresh-rate 1)
(setq symon-delay 1)
(setq symon-sparkline-type 'plain)
