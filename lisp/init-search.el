

(use-package rg
	:ensure t
	:init
	(progn
		(require 'helm-rg)
		(general-define-key
		 :keymaps 'mikus-search-map
		 "d" 'rg-dwim-current-dir
		 "r" 'rg
		 "p" 'rg-dwim-project-dir
		 "P" 'rg-project)
		 ;; "p" 'helm-projectile-rg-at-point
		 ;; "P" 'helm-projectile-rg)
		
		(general-define-key
		 :keymaps 'rg-mode-map
		 "l" 'evil-previous-line
		 "C-c C-p" 'wgrep-change-to-wgrep-mode)
		;; (setq rg-command-line-flags (list "-g '!/**/Generated/*' "))
		;; (setq rg-command-line-flags nil)
		(setq rg-group-result nil)

		)
	)


(provide 'init-search)
