(use-package helm-projectile
	:ensure t
	:diminish
	:init
	(progn
	(projectile-mode)
	(setq projectile-enable-caching t)
	(helm-projectile-on)
	(define-key projectile-command-map (kbd "<ESC>") nil)
	(defvar my-find-command)
	(setq my-find-command "mfd.rb")
	(setq-default projectile-git-command my-find-command)
	(setq-default projectile-generic-command my-find-command)
	(setq projectile-indexing-method 'alien)
	(define-prefix-command 'mikus-tags-map)
	(define-prefix-command 'mikus-search-map)
	(general-define-key
	 :keymaps 'projectile-command-map
	 "ESC" 'keyboard-quit
	 "<tab>" 'projectile-project-buffers-other-buffer)
	(mikus-leader
		:states 'normal
		:keymaps 'override
		"s" 'mikus-search-map
		)
	(general-define-key
	 :keymaps 'projectile-command-map
	 "R" 'projectile-regenerate-tags-async
	 "r" 'mikus-tags-map)
	(general-define-key
	 :keymaps 'mikus-search-map
	 "f" 'fzf-directory
	 "g" 'helm-grep-do-git-grep
	 "a" 'projectile-ag)
		)
	(setq projectile-tags-backend '(etags-select))
	)

(use-package imenu-anywhere
	:ensure t
	:init
	(progn
	(mikus-leader
		:states 'normal
		:keymaps 'override
		"I" 'imenu-anywhere)))

(after-load 'helm-projectile
	(progn

		(defvar idle-game-project-root "c:/ClashOfStreamers/IdleGame/")
		(defvar idle-game-best-folders '( "Assets/#/Sources" "Assets/#/Scripts" "Assets/Editor" ))
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
		
		(defun projectile-regenerate-tags-for-cos-generated-files ()
			"Regenerate tags for this file and append it to the project's TAGS file."
			(interactive)
			(let* ((dir (concat idle-game-project-root "Assets/#/Sources/Generated"))
						 (projectile-tags-command "ctags -Re -f \"%s\" %s -a \"%s\""))
				(projectile-regenerate-tags-async dir)))
		
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
		 "g" 'projectile-regenerate-tags-for-cos-generated-files
		 "i" 'regenerate-idlegame-tags
		 "r" 'regenerate-tags
		 "f" 'projectile-regenerate-tags-for-current-file-async)
		

		)
	)
(provide 'init-projectile)
