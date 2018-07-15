(use-package counsel-projectile
	:ensure t
	:init
	(counsel-projectile-mode)
	(setq projectile-indexing-method 'alien)
	)

(provide 'init-projectile)
