(defun initClashOfStreamers ()
	(cl-pushnew "-g !**/Generated*" rg-command-line-flags)
	(cl-pushnew "-g !**/MessagePack*" rg-command-line-flags))

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
			:query "<<<<<<< HEAD"
			:files current)

		(general-define-key
		 :keymaps 'mikus-search-map
		 "d" 'rg-dwim-current-dir
		 "r" 'rg
		 "p" 'rg-dwim-project-dir
		 "P" 'rg-project
		 "m" 'rg-project-merge-conflicts)
		 ;; "p" 'helm-projectile-rg-at-point
		 ;; "P" 'helm-projectile-rg)
		
		(general-define-key
		 :keymaps 'rg-mode-map
		 "l" 'evil-previous-line
		 "C-c C-p" 'wgrep-change-to-wgrep-mode
		 "C-c C-r" 'rg-reload
		 "n" 'nil
		 "p" 'nil)
		(setq rg-group-result nil)

		)
	:config
	(initClashOfStreamers)
	)


(provide 'init-search)
