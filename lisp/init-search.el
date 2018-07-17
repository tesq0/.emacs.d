

(use-package rg
	:ensure t
	:init
	(progn
		(general-define-key
		 :keymaps 'mikus-search-map
		 "d" 'rg-dwim-current-dir
		 "r" 'rg
		 "p" 'rg-dwim-project-dir
		 "P" 'rg-project))
	)

(provide 'init-search)
