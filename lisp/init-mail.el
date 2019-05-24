(require 'mu4e)
(setq mail-user-agent 'mu4e-user-agent
			mu4e-maildir (expand-file-name "~/.mail/gmail")
			mu4e-get-mail-command "mbsync gmail"
			mu4e-change-filenames-when-moving t
			mu4e-sent-folder "[]")

(provide 'init-mail)

