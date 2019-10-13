(defun setup-php ()
			(setenv "GTAGSLABEL" "pygments")
	)

(use-package php-mode
	:ensure t
	:init
	(add-hook 'php-mode-hook 'setup-php))


(provide 'init-php)
