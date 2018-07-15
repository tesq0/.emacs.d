(use-package counsel-projectile
	:ensure t
	:init
	(counsel-projectile-mode)
	(setq projectile-indexing-method 'alien)
	(setq projectile-tags-command "etags -R -f \"%s\" %s")
	)

(provide 'init-projectile)
