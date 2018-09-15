(use-package general
	:ensure t
	:init
	(progn
		(general-override-mode)
		(general-evil-setup))
	)


(general-define-key
 "C-o" nil)


(provide 'init-general)
