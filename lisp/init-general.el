(use-package general
	:ensure t
	:init
	(progn
		(general-override-mode)
		(general-evil-setup))
	)


(general-define-key
 "C-o" nil)

(general-define-key
 :keymaps 'compilation-mode-map
 :states '(motion normal)
 "C-n" 'compilation-next-error
 "C-p" 'compilation-previous-error
 )


(provide 'init-general)
