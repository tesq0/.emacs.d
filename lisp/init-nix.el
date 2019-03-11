(use-package nix-mode
	:ensure t
	:init
	(setq nix-indent-function 'nix-indent-line))

(defun init-nix-mode ()
	(setq-local company-backends '(company-yasnippet company-files (company-nixos-options company-dabbrev-code company-capf company-keywords) company-dabbrev)))

(use-package company-nixos-options
	:ensure t
	:init
	(add-hook 'nix-mode-hook 'init-nix-mode))

(provide 'init-nix)
