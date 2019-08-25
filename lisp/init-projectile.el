(use-package helm-projectile
	:ensure t
	:diminish
	:init
	(progn
		(projectile-mode)
		(setq projectile-enable-caching t
					helm-projectile-fuzzy-match nil)
		(helm-projectile-on)
		(define-key projectile-command-map (kbd "<ESC>") nil)
		(defvar my-find-command)
		(setq my-find-command (or (and sys/win32p "mfd.rb") "fd --hidden --exclude '.git' -t f . -0"))
		(setq-default projectile-git-command my-find-command)
		(setq-default projectile-generic-command my-find-command)
		(setq projectile-indexing-method 'alien)
		(setq projectile-tags-backend 'auto)
		(define-prefix-command 'mikus-tags-map)
		(define-prefix-command 'mikus-search-map)
		(general-define-key
		 :keymaps 'projectile-command-map
		 "ESC" 'keyboard-quit
		 "<tab>" 'projectile-project-buffers-other-buffer)

		(defvar save-project-commands '(save-all-buffers))

		(defun save--project ()
			(interactive)
			(if (listp save-project-commands)
					(dolist (fn save-project-commands)
						(funcall fn))))

		(mikus-leader "s" 'mikus-search-map)

		(general-define-key
		 :keymaps 'projectile-command-map
		 "R" 'projectile-regenerate-tags-async
		 "r" 'mikus-tags-map
		 "s" 'save--project)
		(general-define-key
		 :keymaps 'mikus-search-map
		 "f" 'fzf
		 "g" 'helm-grep-do-git-grep
		 "a" 'projectile-ag)
		)
	(setq projectile-tags-backend '(etags-select))
	)

(use-package imenu-anywhere
	:ensure t
	:init
	(progn
		(mikus-leader "I" 'imenu-anywhere)))

;; Tags
(after-load 'helm-projectile

	(defvar projectile-custom-ignored-files '())

	(defun tags-custom-ignored-files ()
			projectile-custom-ignored-files)

	(defun projectile-tags-exclude-patterns ()
		"Return a string with exclude patterns for ctags."
		(mapconcat (lambda (pattern) (format "--exclude=\"%s\""
																				 (directory-file-name pattern)))
							 (append (tags-custom-ignored-files) (projectile-ignored-directories-rel)) " "))


	(defun projectile-regenerate-tags-async (&optional files append)
		"Regenerate the project's [e|g]tags.  Optionally specify FILES."
		(interactive)
		(let* ((project-root (projectile-project-root))
					 (tags-exclude (projectile-tags-exclude-patterns))
					 (default-directory project-root)
					 (tags-file (expand-file-name projectile-tags-file-name))
					 (command (format projectile-tags-command tags-file tags-exclude (or files default-directory))))
			(message "regenerate tags command: %s" command)
			(async-shell-command command)))


	(general-define-key
	 :keymaps 'mikus-tags-map
	 "r" 'projectile-regenerate-tags
	 "R" 'projectile-regenerate-tags-async)


	)
(provide 'init-projectile)
