
;;C++
(use-package irony
	:ensure t
	:init
	(progn
		(setq irony--server-executable (concat (getenv "HOME") "/.nix-profile/bin/irony-server"))
		(add-hook 'c++-mode-hook 'irony-mode)
		(add-hook 'c-mode-hook 'irony-mode)
		(add-hook 'objc-mode-hook 'irony-mode)
		(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package flycheck-irony
	:after irony
	:ensure t
	:init
	(flycheck-irony-setup))

(use-package irony-eldoc
	:after irony
	:ensure t)

(provide 'init-c)
