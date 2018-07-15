
(when sys/win32p
	(progn

		(setq cygwin-bin "c:\\cygwin64\\bin")
		(setq exec-path
					'(cygwin-bin))))

(provide 'init-system)
