(defun setup-php ()
			(setenv "GTAGSLABEL" "pygments")
			(setq-local c-basic-offset 4)
			(lsp)
	)

(use-package php-mode
	:ensure t
	:init
	(add-hook 'php-mode-hook 'setup-php)
	)


(provide 'init-php)
