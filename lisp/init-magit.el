;; magit package

(defun initMagit ()
	(setq evil-magit-want-horizontal-movement t)

	(general-define-key
	 :keymaps 'magit-status-mode-map
	 "j" nil)

	(define-prefix-command 'mikus-magit-map)
	(define-prefix-command 'mikus-magit-blame-map)

	(defun setup-default-blame-style (type)
		(setq-local magit-blame--style (cadr magit-blame-styles)))

	(advice-add 'magit-blame--pre-blame-setup :before #'setup-default-blame-style)

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
	 "s" 'magit-blame-cycle-style
	 "c" 'magit-blame-show-commit
	 "q" 'magit-blame-quit)

	(general-define-key
	 :keymaps 'transient-map
	 "<escape>" 'transient-quit-one)

	(general-define-key
	 :keymaps 'mikus-magit-map
	 "s" 'magit-status
	 "e" 'magit-ediff
	 "d" 'magit-diff
	 "f" 'magit-find-file
	 "c" 'vc-find-conflicted-file
	 "b" 'magit-blame))

(use-package evil-magit
	:after magit
	:demand t
	:ensure nil
	:quelpa (evil-magit :fetcher github :repo "tesq0/evil-magit"))

(use-package magit
	:ensure t
	:init (initMagit))

(use-package git-gutter
	:ensure t
	:init
	(progn
		(define-prefix-command 'mikus-gitgutter-map)
		(general-define-key
		 :keymaps 'mikus-gitgutter-map
		 "n" 'git-gutter:next-hunk
		 "p" 'git-gutter:previous-hunk
		 "u" 'git-gutter:revert-hunk
		 "s" 'git-gutter:stage-hunk
		 "d" 'git-gutter:popup-diff)
		(general-define-key
		 :keymaps 'mikus-magit-map
		 "g" 'mikus-gitgutter-map)
		(add-hook 'prog-mode-hook #'git-gutter-mode)))

(after-load 'smerge-mode
	(progn
		 (message "smerge mode loaded")
		 (general-define-key
			:keymaps 'smerge-mode-map
			"C-c m" (lookup-key smerge-mode-map (kbd "C-c ^")))))
	

(provide 'init-magit)
