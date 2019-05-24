(use-package mu4e
	:ensure nil
	:init
	(progn
		(setq mail-user-agent 'mu4e-user-agent
					mu4e-maildir (expand-file-name "~/.mail")
					mu4e-get-mail-command "mbsync gmail"
					mu4e-change-filenames-when-moving t)))

(provide 'init-mail)

