(use-package ido-vertical-mode
	:ensure t
	:init
	(ido-vertical-mode)
	:config
	(setq ido-vertical-define-keys 'C-n-and-C-p-only)
	;; (general-define-key
	;;  :keymap 'ido-buffer-completion-map
	;;  "C-n" 'select-next
	;; )
	)

(provide 'init-ido)


