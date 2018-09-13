(use-package dictionary
	:ensure t
	:init
	(progn
		(mikus-leader
			:states '(normal motion visual)
			:keymaps 'override
			"d" 'dictionary-search)
		)
	:config
	(set-face-foreground 'dictionary-reference-face "CornflowerBlue")
	)

(provide 'init-dict)
