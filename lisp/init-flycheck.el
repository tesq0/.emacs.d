(use-package flycheck
  :ensure t
	:hook (prog-mode . flycheck-mode)
	:config
	(setq flycheck-highlighting-mode 'symbols)
	(setq flycheck-check-syntax-automatically '(mode-enabled save))
)

(provide 'init-flycheck)
