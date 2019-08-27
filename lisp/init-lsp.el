(use-package lsp-mode
	:ensure t
	:config
	(setq lsp-enable-indentation nil
				lsp-inhibit-message t
				lsp-eldoc-render-all t
				lsp-highlight-symbol-at-point nil))

(use-package company-lsp
	:after company
	:ensure t
	:config
	(setq company-lsp-cache-candidates t))

(use-package lsp-ui
	:ensure t
	:config
	(setq lsp-ui-sideline-enable nil
				lsp-ui-doc-enable nil
				lsp-ui-flycheck-enable t
				lsp-prefer-flymake nil
				lsp-ui-imenu-enable t
				lsp-ui-sideline-ignore-duplicate t))

(provide 'init-lsp)
