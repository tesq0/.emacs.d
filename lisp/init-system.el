(defun windowsPerformanceTweaks()
	;; Windows performance tweaks
	;;
	(when (boundp 'w32-pipe-read-delay)
		(setq w32-pipe-read-delay 0))
	;; Set the buffer size to 64K on Windows (from the original 4K)
	(when (boundp 'w32-pipe-buffer-size)
		(setq irony-server-w32-pipe-buffer-size (* 64 1024))))

(when sys/win32p
	(progn

		(windowsPerformanceTweaks)
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
