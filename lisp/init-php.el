(defun setup-php ()
			(setenv "GTAGSLABEL" "pygments")
			(setq-local c-basic-offset 4)
			(lsp)
			(setq-local company-backends '(company-files (company-dabbrev-code :with company-lsp company-yasnippet company-keywords) ))
	)

(use-package php-mode
	:ensure t
	:init
	(add-hook 'php-mode-hook 'setup-php)
	)


(provide 'init-php)
