;; PuTTy fix. Ugly. bad. But it works. (Good)
;; For end key
(define-key global-map "\M-[1~" 'beginning-of-line)
(define-key global-map [select] 'end-of-line)
