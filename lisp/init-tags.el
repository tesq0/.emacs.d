(use-package ggtags
	:hook (csharp-mode . ggtags-mode)
	:ensure t
	:init
	(progn
		(define-prefix-command 'helm-gtags-map)
		(add-to-list 'xref-backend-functions 'ggtags--xref-backend)
		(setq ggtags-highlight-tag nil)
		(general-define-key
		 :keymaps 'helm-gtags-map
		 "P"  'ggtags-visit-project-root
		 "h"  'ggtags-view-tag-history
		 "f"  'ggtags-find-file
		 "g"  'ggtags-grep
		 "s"  'ggtags-find-other-symbol
		 "r"  'ggtags-find-reference
		 "`"  'ggtags-save-to-register
		 "t"  'ggtags-find-tag-dwim
		 "q"  'ggtags-query-replace
		 "n"  'ggtags-next-mark
		 "p"  'ggtags-prev-mark
		 "d"  'ggtags-show-definition)
		(mikus-leader
			:states '(normal motion)
			"t" 'helm-gtags-map)))


(provide 'init-tags)
