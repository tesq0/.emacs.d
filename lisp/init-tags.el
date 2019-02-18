(use-package ggtags
	:hook (csharp-mode . ggtags-mode)
	:ensure t
	:init
	(progn
		(define-prefix-command 'helm-gtags-map)
		(setq ggtags-highlight-tag nil
					ggtags-split-window-function nil
					ggtags-global-window-height nil
					ggtags-completing-read-function 'helm-completing-read-default
					ggtags-auto-jump-to-match nil)

		(general-define-key
		 :states '(normal motion)
		 "<mouse-2>" 'ggtags-find-tag-mouse)

		(general-define-key
		 :keymaps 'ggtags-mode-map
		 "M-." nil)

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
