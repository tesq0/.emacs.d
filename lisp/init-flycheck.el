(use-package flycheck
  :ensure t
	:hook (prog-mode . flycheck-mode)
	:config
	(setq flycheck-highlighting-mode 'symbols)
	(setq flycheck-check-syntax-automatically '(mode-enabled save))
)

(use-package flycheck-pos-tip
	:ensure t
	:after flycheck
	:init
	(progn
	(flycheck-pos-tip-mode)))

(provide 'init-flycheck)
