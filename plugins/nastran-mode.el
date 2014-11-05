;; (require 'vvb-mode)
(load "~/.emacs.d/add-ins/vvb-mode")

(setq-default vvb-columns '(9 17 25 33 41 49 57 65 73 81)
	      vvb-sticky-p nil
	      vvb-permanent-p t)

(add-to-list 'auto-mode-alist (("\\.bdf\\'" . nastran-mode)
			       ("\\.dat\\'" . nastran-mode)
			       ("\\.bas\\'" . nastran-mode)))

(provide 'nastran-mode)
