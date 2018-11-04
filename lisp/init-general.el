(use-package general
	:ensure t
	:config
	(general-override-mode)
	(general-evil-setup))

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
 "C-o" nil)


(provide 'init-general)
