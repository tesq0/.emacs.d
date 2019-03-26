(use-package general
	:ensure t
	:config
	(general-override-mode)
	(general-evil-setup))

(after-load 'general
	(progn
		(general-define-key
		 :keymaps 'package-menu-mode-map
		 "k" 'evil-next-line
		 "l" 'evil-previous-line
		 "C-k" 'evil-scroll-down
		 "C-l" 'evil-scroll-up
		 "`" 'evil-paste-from-register
		 ";" 'evil-forward-char
		 "j" 'evil-backward-char
		 "C-s" 'helm-occur
		 "q" 'quit-window
		 "C-w" 'evil-window-map)

		(general-define-key
		 "C-o" nil
		 "<kp-add>" 'increment-number-at-point
		 "<kp-subtract>" 'decrement-number-at-point
		 )

		(general-define-key
		 :keymaps 'eww-mode-map
		 :states '( normal motion )
		 "<" 'eww-back-url
		 ">" 'eww-forward-url)

		(general-define-key
		 :keymaps 'Info-mode-map
		 :states '(motion)
		 "j" 'evil-backward-char
		 "k" 'evil-next-line
		 "l" 'evil-previous-line
		 ";" 'evil-forward-char
		 "h" 'Info-history-back
		 )

		(general-define-key
		 :keymaps 'help-mode-map
		 :states '(normal)
		 "q" 'quit-window)

		(general-define-key
		 :keymaps 'eww-mode-map
		 "C-c n" 'eww-next-url
		 "C-c p" 'eww-previous-url
		 "C-c b" 'eww-switch-to-buffer
		 "C-c s" 'eww-view-source
		 "C-c d" 'eww-download
		 "C-c r" 'eww-reload
		 "C-c R" 'eww-readable
		 "C-c m" 'eww-add-bookmark
		 "C-c B" 'eww-list-bookmarks
		 "C-c f" 'eww-toggle-fonts
		 "C-c E" 'eww-browse-with-external-browser
		 "C-c y" 'eww-copy-page-url
		 "C-c g" 'eww)

		))




(provide 'init-general)
