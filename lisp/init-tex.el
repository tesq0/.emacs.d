(use-package auctex
	:ensure t
	:init
	(progn
		(setq TeX-auto-save t)
		(setq TeX-parse-self t)
		(setq-default TeX-master nil)
		(add-hook 'LaTeX-mode-hook 'visual-line-mode)
		(add-hook 'LaTeX-mode-hook 'flyspell-mode)
		(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
		(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
		(setq reftex-plug-into-AUCTeX t)))

(use-package company-auctex
	:after auctex
	:ensure t)


(provide 'init-tex)
