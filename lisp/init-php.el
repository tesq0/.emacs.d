(defun setup-php ()
			(setenv "GTAGSLABEL" "pygments")
			(setq-local c-basic-offset 4)
			(lsp)
			(yas-minor-mode)
			(setq-local company-backends '(company-files (company-dabbrev-code :with company-capf company-yasnippet company-keywords) ))
			(setq-local company-manual-completion-fn #'company-capf)
	)

(use-package php-mode
	:ensure t
	:init
	(add-hook 'php-mode-hook 'setup-php)
	)

(use-package geben
	:ensure t)

(provide 'init-php)
