(use-package ggtags
	:hook (csharp-mode . ggtags-mode)
	:ensure t
	:init
	(progn
		(define-prefix-command 'helm-gtags-map)
		(setq ggtags-highlight-tag nil
					ggtags-split-window-function nil
					ggtags-global-window-height nil
					ggtags-completing-read-function nil
					ggtags-auto-jump-to-match 'first)

		(defun compilation-maybe-halt-auto-jump (buffer pos)
			"Halt jumping to first match in ggtags-global-mode if more that 1 results."
			(let* ((bname (buffer-name buffer))
						 (ggtags (string-equal bname "*ggtags-global*")))
				(when ggtags
					(with-current-buffer buffer
						(let* ((lines (count-lines pos (point-max)))
									 (halt (> lines 4))) ;; more than 4 seems to mean more than 1 match
							;; (message (format "output lines %s halt? %s" lines halt))
							(when halt
								(setq compilation-auto-jump-to-first-error nil)))))))

		(defun ggtags-query-tags (name)
			(interactive (list (ggtags-read-tag 'definition 1)))
			(ggtags-find-tag 'definition "--" (shell-quote-argument name)))

		(advice-add 'compilation-auto-jump :before #'compilation-maybe-halt-auto-jump)

		(general-define-key
		 :states '(normal motion)
		 "<mouse-2>" 'ggtags-find-tag-mouse)

		(general-define-key
		 :keymaps '( ggtags-mode-map ggtags-navigation-map )
		 "M-." nil
		 "M->" nil)

		(general-define-key
		 :keymaps 'ggtags-mode-map
		 :states '(motion normal visual)
		 "gd" 'ggtags-find-definition)

		(general-define-key
		 :keymaps 'ggtags-global-mode-map
		 "l" 'evil-previous-line
		 "C-c C-p" 'wgrep-change-to-wgrep-mode
		 "C-c C-r" 'nil)

		(general-define-key
		 :keymaps 'helm-gtags-map
		 "P"  'ggtags-visit-project-root
		 "h"  'ggtags-view-tag-history
		 "f"  'ggtags-find-file
		 "g"  'ggtags-grep
		 "s"  'ggtags-find-other-symbol
		 "r"  'ggtags-find-reference
		 "`"  'ggtags-save-to-register
		 "t"  'ggtags-query-tags
		 "q"  'ggtags-query-replace
		 "n"  'ggtags-next-mark
		 "p"  'ggtags-prev-mark
		 "d"  'ggtags-show-definition)

		(mikus-leader "t" 'helm-gtags-map)))


(provide 'init-tags)
