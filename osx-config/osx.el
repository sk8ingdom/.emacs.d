;; Disable toolbar (to enable it again, replace the -1 with 1)
(tool-bar-mode nil)

;; Full screen by default
;; (set-frame-parameter nil 'fullscreen 'fullboth)

;; Maximized by default
(setq initial-frame-alist
      `((left . 0) (top . 420)
        (width . 207) (height . 31)))

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
;; Requires the following in System Preferences > Keyboard > Modifer Keys
;;
;; Apple Internal Keyboard / Trackpad:
;;
;;    Caps Lock   : Control
;;    Control Key : Command
;;    Option Key  : Control
;;    Command Key : Option
;;
;; daskeyboard:
;;
;;   Caps Lock   : Control
;;   Control Key : Command
;;   Option Key  : Option
;;   Command Key : Control
;;
;; Sets the remapped Command key to Meta
(setq mac-command-modifier 'control)
;; Sets the remapped Option key to Control
(setq mac-option-modifier 'meta)
;; Sets the remapped control key to Control
(setq mac-control-modifier 'control)
