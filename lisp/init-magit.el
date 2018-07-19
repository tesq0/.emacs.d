
;; magit package

(use-package magit
	:ensure t
	:init
	(progn

		(define-prefix-command 'mikus-magit-map)
		(define-prefix-command 'mikus-magit-blame-map)

		(setq magit-blame--style (cadr magit-blame-styles))
		(general-define-key
		 :keymaps 'mikus-magit-blame-map
		 "C-S-c" 'magit-blame-copy-hash
		 "b" 'magit-blame
		 "B" 'magit-blame-popup
		 "n" 'magit-blame-next-chunk
		 "N" 'magit-blame-next-chunk-same-commit
		 "p" 'magit-blame-previous-chunk
		 "P" 'magit-blame-previous-chunk-same-commit
		 "r" 'magit-blame-removal
		 "f" 'magit-blame-reverse
		 "c" 'magit-blame-show-commit
		 "q" 'magit-blame-quit
		 )
		(general-define-key
			:keymaps 'mikus-magit-map
			"g" 'magit-status
			"b" 'mikus-magit-blame-map)
		)
	:config
	(define-key magit-mode-map (kbd "<escape>") 'magit-mode-bury-buffer)
	(define-key magit-mode-map (kbd "C-g") 'magit-mode-bury-buffer)
	;; SMERGE
	(define-key smerge-mode-map (kbd "C-c m") (lookup-key smerge-mode-map (kbd "C-c ^")))
	)

(provide 'init-magit)
