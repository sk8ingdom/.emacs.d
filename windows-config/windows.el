;; Disable toolbar
(tool-bar-mode nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Courier New" :foundry "outline" :slant normal :weight normal :height 83 :width normal)))))

;; Set the frame size and location
(interactive)
(set-frame-position (selected-frame) 1600 -770)
;; (set-frame-height (selected-frame) 69)
;; (set-frame-width (selected-frame) 164)
(set-frame-size (selected-frame) 164 69)
;; (add-to-list 'default-frame-alist '(height . 164))
;;(add-to-list 'default-frame-alist '(width . 69))
