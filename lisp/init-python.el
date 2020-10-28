(use-package python-mode
	:commands python-mode
	:mode ("\\.py\\'" . python-mode)
	:interpreter (("python"  . python-mode)
								("python3" . python-mode))
	:preface
	(defun setup-py ()
		(require 'lsp-pyls)
		(yas-minor-mode-on)
		(lsp-deferred)
		)

	(defun toggle-py-formating ()

		(if (and (eq major-mode 'python-mode) (memq 'lsp-mode minor-mode-list))
				(add-hook 'before-save-hook #'lsp-format-buffer)
			(remove-hook 'before-save-hook #'lsp-format-buffer)))
	
	:hook ((python-mode . setup-py))
	:config
	(add-hook 'after-change-major-mode-hook #'toggle-py-formating)
	
)

(provide 'init-python)
