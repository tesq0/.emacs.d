(use-package helm-projectile
	:ensure t
	:diminish
	:init
	(progn
	(projectile-mode)
	(helm-projectile-on)
	(define-key projectile-command-map (kbd "<ESC>") nil)
	(setq projectile-indexing-method 'hybrid)
	(setq projectile-globally-ignored-file-suffixes  '("png" "unity" "tga" "psd" "anim" "prefab" "mat" "meta"))
	(setq grep-find-ignored-files (append grep-find-ignored-files '("*.meta" "*.png" "*.unity" "*.tga" "*.psd" "*.anim" "*.prefab" "*.mat")))
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
	 "R" 'projectile-regenerate-tags-async)
	(general-define-key
	 :keymaps 'mikus-search-map
	 "f" 'fzf-directory
	 "g" 'helm-grep-do-git-grep
	 "a" 'projectile-ag)
		)

	)


;;;###autoload
(defun projectile-regenerate-tags-async ()
	"Regenerate the project's [e|g]tags."
	(interactive)
	(if (and (boundp 'ggtags-mode)
					 (memq projectile-tags-backend '(auto ggtags)))
			(progn
				(let* ((ggtags-project-root (projectile-project-root))
							 (default-directory ggtags-project-root))
					(ggtags-ensure-project)
					(ggtags-update-tags t)))
		(let* ((project-root (projectile-project-root))
					 (tags-exclude (projectile-tags-exclude-patterns))
					 (default-directory project-root)
					 (tags-file (expand-file-name projectile-tags-file-name))
					 (command (format projectile-tags-command tags-file tags-exclude))
					 shell-output exit-code)
			(with-temp-buffer
				(setq exit-code
							(async-shell-command command nil (current-buffer))
							shell-output (projectile-trim-string
														(buffer-substring (point-min) (point-max)))))
			(unless (zerop exit-code)
				(error shell-output))
			(visit-tags-table tags-file)
			(message "Regenerated %s" tags-file))))


(provide 'init-projectile)
