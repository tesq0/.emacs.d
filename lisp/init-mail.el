(require 'mu4e)
(setq mail-user-agent 'mu4e-user-agent
      mu4e-maildir "~/.mail"
      mu4e-get-mail-command "mbsync gmail"
      mu4e-change-filenames-when-moving t
      mu4e-attachment-dir "~/Downloads"
      mu4e-drafts-folder "/gmail/Drafts"
      mu4e-refile-folder "/gmail/Archive"
      mu4e-trash-folder "/gmail/Trash"
      mu4e-sent-folder "/gmail/Sent Mail"
      mu4e-view-show-images t
      mu4e-view-show-addresses t
      browse-url-generic-program "firefox"
      mu4e-use-fancy-chars t)

(setq mu4e-maildir-shortcuts
      '(("/gmail/INBOX" . ?i)
	("/gmail/Trash" . ?t)
	("/gmail/Drafts" . ?d)
	("/gmail/Sent Mail" . ?s)))

(global-set-key (kbd "<f12>") 'mu4e)

(provide 'init-mail)

