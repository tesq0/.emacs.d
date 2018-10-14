(when sys/win32p
	(progn

		;; Make sure Unix tools are in front of `exec-path'
		(let ((bash (executable-find "bash")))
			(when bash
				(push (file-name-directory bash) exec-path)))

		;; Update PATH from exec-path
		(let ((path (mapcar 'file-truename
												(append exec-path
																(split-string (getenv "PATH") path-separator t)))))

			(setenv "PATH" (mapconcat 'identity (delete-dups path) path-separator)))

		(setq user-emacs-directory (concat (getenv "HOME") "\\.emacs.d"))))


(provide 'init-system)
