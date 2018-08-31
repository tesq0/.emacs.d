

(use-package rg
	:ensure t
	:init
	(progn
		(general-define-key
		 :keymaps 'mikus-search-map
		 "d" 'rg-dwim-current-dir
		 "r" 'rg
		 "p" 'rg-dwim-project-dir
		 "P" 'rg-project)
		(general-define-key
		 :keymaps 'rg-mode-map
		 "l" 'evil-previous-line)
		;; (setq rg-command-line-flags (list "-g '!/**/Generated/*' "))
		;; (setq rg-command-line-flags nil)
		(setq rg-group-result nil)

		)
	)

(provide 'init-search)
