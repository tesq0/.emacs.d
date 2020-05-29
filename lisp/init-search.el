(defun initClashOfStreamers ()
	(cl-pushnew "-g !**/Generated*" rg-command-line-flags)
	(cl-pushnew "-g !**/MessagePack*" rg-command-line-flags))

(use-package fzf
	:ensure t
	:init
	(progn

		(require 'fzf)
		
		(require 'fzf)
		(defun fzf ()
			"Starts a fzf session."
			(interactive)
			(if (fboundp #'projectile-project-root)
					(fzf/start (condition-case err
												 (or (projectile-project-root) default-directory)
											 (error
												default-directory)))
				(fzf/start default-directory)))

		(defun fcd ()
			(interactive)
			(fzf/start "" (format "fd -t d %s" default-directory)))
		
		(general-define-key
		 :keymaps 'mikus-search-map
		 "f" 'fzf)

		))

(use-package rg
	:ensure t
	:init
	(progn

		(defun rg-reload ()
			(interactive)
			(rg-rerun))

		(defvar rg-cur-regexp "Regexp of rg's current search pattern" nil)

		(defun rg-maybe-set-evil-search-pattern (&rest args)
			(let* ((search-pattern (cl-struct-slot-value 'rg-search 'pattern rg-cur-search))
						 (regexp (regexp-quote search-pattern)))
				(message "auto jump %s" regexp)
				(setq evil-ex-search-pattern (list regexp t t))
				)
			)

		(advice-add 'rg-filter :after #'rg-maybe-set-evil-search-pattern)

		(rg-define-search rg-project-merge-conflicts
			:dir project
			:query "<<<<<<<"
			:files current)

		(general-define-key
		 :keymaps 'mikus-search-map
		 "d" 'rg-dwim-current-dir
		 "r" 'rg
		 "g" 'find-grep
		 "p" 'rg-dwim-project-dir
		 "P" 'rg-project
		 "m" 'rg-project-merge-conflicts)

		(general-unbind rg-mode-map
			"e" "n" "p")

		(general-define-key
		 :keymaps 'rg-mode-map
		 "l" 'evil-previous-line
		 "C-c C-p" 'wgrep-change-to-wgrep-mode
		 "C-c C-r" 'rg-reload)

		(general-define-key
		 :keymaps 'grep-mode-map
		 "C-c C-p" 'wgrep-change-to-wgrep-mode)
		
		(setq rg-group-result nil)

		)
	)

(provide 'init-search)
