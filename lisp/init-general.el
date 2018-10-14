(use-package general
	:ensure t
	:config
	(general-override-mode)
	(general-evil-setup))

(general-define-key
 :keymaps 'package-menu-mode-map
 "k" 'evil-next-line
 "l" 'evil-previous-line
 "`" 'evil-paste-from-register
 ";" 'evil-forward-char
 "j" 'evil-backward-char
 "C-s" 'swiper-helm
 )



(general-define-key
 "C-o" nil)


(provide 'init-general)
