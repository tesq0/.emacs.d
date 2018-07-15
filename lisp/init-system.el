(when sys/win32p
	(progn

		(setq cygwin-exec-path "c:/cygwin64/bin")
		(setq cygwin-PATH "C:\\cygwin64\\bin")
		(if (file-exists-p cygwin-PATH)
				(progn
					(cl-pushnew cygwin-exec-path exec-path)
					(setenv "PATH" (concat cygwin-PATH ";" (getenv "PATH")))))))

(provide 'init-system)
