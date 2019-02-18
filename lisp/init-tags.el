(use-package helm-gtags
	:ensure t
	:hook (csharp-mode . helm-gtags-mode)
	:init
	(progn
		(define-prefix-command 'helm-gtags-map)
		(general-define-key
		 :keymaps 'helm-gtags-map
		 "c"  'helm-gtags-create-tags
		 "h"  'helm-gtags-display-browser
		 "P"  'helm-gtags-find-files
		 "f"  'helm-gtags-parse-file
		 "g"  'helm-gtags-find-pattern
		 "s"  'helm-gtags-find-symbol
		 "r"  'helm-gtags-find-rtag
		 "t"  'helm-gtags-find-tag
		 "d"  'helm-gtags-find-tag)
		(mikus-leader
			:states '(normal motion)
			"t" 'helm-gtags-map))
	)

(provide 'init-tags)
