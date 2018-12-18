(defun setup-ycmd ()
	"Setup Ycmd completion server."
	(set-variable 'ycmd-server-command '("python" "-u" "C:\\Users\\mikol\\AppData\\Roaming\\.emacs.d\\ycmd\\ycmd"))
	(set-variable 'ycmd-global-config "C:\\Users\\mikol\\.ycm_extra_conf.py"))

(use-package ycmd
	:ensure t
	:init (setup-ycmd))

(use-package company-ycmd
	:after company
	:init (company-ycmd-setup))

(use-package flycheck-ycmd
	:after flycheck
	:init (flycheck-ycmd-setup))

(use-package ycmd-eldoc
	:disabled t
	:commands (ycmd-eldoc-setup)
	:after eldoc
	:init
	(add-hook 'ycmd-mode-hook #'ycmd-eldoc-setup))


(provide 'init-ycm)

