
;;C++
(use-package irony
	:ensure t
	:init
	(progn
		(add-hook 'c++-mode-hook 'irony-mode)
		(add-hook 'c-mode-hook 'irony-mode)
		(add-hook 'objc-mode-hook 'irony-mode)
		(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package flycheck-irony
	:after irony
	:ensure t)

(use-package irony-eldoc
	:after irony
	:ensure t)

;; bojective-c
(use-package flycheck-objc-clang
	:ensure t
	:init
	(flycheck-objc-clang-setup))

(provide 'init-c)
