(use-package google-translate
	:ensure t
	:init
	(progn
		(setq
		 google-translate-default-source-language "English"
		 google-translate-default-target-language "Polish")))
(use-package dictionary
	:ensure t
	:init
	(progn
		(define-prefix-command 'dict-map)
		(general-define-key
		 :keymaps 'dict-map
		 "d" 'dictionary-search
		 "g" 'google-translate-smooth-translate)
		(mikus-leader
			:states '(normal motion visual)
			:keymaps 'override
			"d" 'dict-map)
		)
	:config
	(set-face-foreground 'dictionary-reference-face "CornflowerBlue")
	)

(provide 'init-dict)
