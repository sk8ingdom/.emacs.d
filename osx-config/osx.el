;; Disable toolbar (to enable it again, replace the -1 with 1)
(tool-bar-mode -1)

;; Full screen by default
;; (set-frame-parameter nil 'fullscreen 'fullboth)

;; Maximized by default
(setq initial-frame-alist
      `((left . 0) (top . 0)
        (width . 207) (height . 56)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Andale Mono" :foundry "outline" :slant normal :weight normal :height 100 :width normal)))))

;; Mapping Control ＆ Alt Keys
(setq mac-command-modifier 'meta) ; sets the Command key to Meta
(setq mac-option-modifier 'control) ; sets the Option key to Control
(setq mac-control-modifier 'control) ; sets the Control key to Control
