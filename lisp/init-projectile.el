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
		(setq my-find-command (or (and sys/win32p "mfd.rb") "fd -t f . -0"))
		(setq-default projectile-git-command my-find-command)
		(setq-default projectile-generic-command my-find-command)
		(setq projectile-indexing-method 'alien)
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

;; Find files source
(after-load 'helm-projectile
	(defun mikus-helm-projectile-find-file ()
		"Find file at point based on context."
		(interactive)
		(let* ((project-root (projectile-project-root))
					 (project-files (projectile-current-project-files))
					 (files (projectile-select-files project-files)))
			(if (= (length files) 1)
					(find-file (expand-file-name (car files) (projectile-project-root)))
				(helm :sources (helm-build-sync-source "Projectile files"
												 :candidates (if (> (length files) 1)
																				 (helm-projectile--files-display-real files project-root)
																			 (helm-projectile--files-display-real project-files project-root))
												 :fuzzy-match helm-projectile-fuzzy-match
												 :action-transformer 'helm-find-files-action-transformer
												 :keymap helm-projectile-find-file-map
												 :help-message helm-ff-help-message
												 :mode-line helm-read-file-name-mode-line-string
												 :action helm-projectile-file-actions
												 :persistent-action #'helm-projectile-file-persistent-action
												 :match-part (lambda (c) (helm-basename c))
												 :persistent-help "Preview file")
							:buffer "*helm projectile*"
							:truncate-lines helm-projectile-truncate-lines
							:prompt (projectile-prepend-project-name "Find file: ")))))

	(setq projectile-switch-project-action 'mikus-helm-projectile-find-file)

	(general-define-key
	 :keymaps 'projectile-command-map
	 "f" 'mikus-helm-projectile-find-file)
	)

;; Tags
(after-load 'helm-projectile

	(defvar idle-game-project-root "/media/LINUX/mikus/ClashOfStreamers/IdleGame/")
	(defvar idle-game-best-folders '( "Assets/#/Sources" "Assets/#/Scripts" "Assets/Editor" "Assets/#/Libraries" ))
	(defvar idle-game-ignored-files (append grep-find-ignored-files '("*.asset" "*.java" "*.m" "MessagePack")))

	(defvar projectile-custom-ignored-files '())

	(defun tags-custom-ignored-files ()
		(if (string-equal ( projectile-project-root ) idle-game-project-root)
				idle-game-ignored-files
			projectile-custom-ignored-files))

	(defun idle-game-folders ()
		(mapconcat (lambda (path) (format "\"%s\"" (concat idle-game-project-root path))) idle-game-best-folders " "))

	(defun regenerate-idlegame-tags ()
		(interactive)
		(let* ((dirs (idle-game-folders))
					 (projectile-tags-command "ctags -Re -f \"%s\" %s %s"))
			(projectile-regenerate-tags-async dirs)))

	(defun regenerate-idlegame-gtags (&optional dirs)
		"First output the files in DIRS we want to parse to gtags.files, then run gtags in idle-game-project-root."
		(interactive)
		(let* ((directories (or dirs (idle-game-folders)))
					 (default-directory (projectile-project-root))
					 (find-command (format "fd .cs %s > %sgtags.files" directories default-directory))
					 (gtags-command (format "gtags -v --gtagslabel pygments %s" (directory-file-name default-directory))))
			(message (format "find files command %s" find-command))
			(shell-command find-command)
			(shell-command gtags-command)
			))

	(defun projectile-regenerate-tags-for-current-file-async ()
		"Regenerate tags for this file."
		(interactive)
		(let* ((current-file (buffer-file-name))
					 (projectile-tags-command "ctags -Re -f \"%s\" %s -a \"%s\""))
			(projectile-regenerate-tags-async current-file)))

	(defun projectile-tags-exclude-patterns ()
		"Return a string with exclude patterns for ctags."
		(mapconcat (lambda (pattern) (format "--exclude=\"%s\""
																				 (directory-file-name pattern)))
							 (append (tags-custom-ignored-files) (projectile-ignored-directories-rel)) " "))

	(defun regenerate-tags ()
		(interactive)
		(if (string-equal ( projectile-project-root ) idle-game-project-root)
				(regenerate-idlegame-tags)
			(projectile-regenerate-tags-async)))

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
	 "i" 'regenerate-idlegame-tags
	 "g" 'regenerate-idlegame-gtags
	 "r" 'regenerate-tags
	 "f" 'projectile-regenerate-tags-for-current-file-async)


	)
(provide 'init-projectile)
