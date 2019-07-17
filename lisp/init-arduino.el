(defun setup-arduino ()
	(setq-local company-backends '(company-files (company-dabbrev-code :with company-arduino company-yasnippet company-keywords) )))

(use-package arduino-mode
	:ensure t)

(use-package company-arduino
	:after arduino-mode
	:hook (arduino-mode . setup-arduino)
	:init
	(setq irony-disabled-modes '(arduino-mode)))

(provide 'init-arduino)

