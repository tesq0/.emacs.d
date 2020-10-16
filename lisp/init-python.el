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
		;; (setq-local company-backends '(company-files (company-dabbrev-code :with company-lsp company-yasnippet company-keywords)))
		)

	:hook ((python-mode . setup-py))

	:config
	(add-hook #'python-mode-hook
						#'(lambda () (add-hook #'before-save-hook #'lsp-format-buffer nil t)))

)

(provide 'init-python)
