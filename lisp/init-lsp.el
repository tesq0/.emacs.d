(use-package lsp-mode
	:ensure nil
	:quelpa
	(lsp-mode :fetcher github :repo "emacs-lsp/lsp-mode" :branch "6.3.1")
	:config
	(setq lsp-enable-indentation nil
				lsp-inhibit-message t
				lsp-eldoc-render-all t
				lsp-enable-file-watchers nil
				lsp-highlight-symbol-at-point nil))

(use-package company-lsp
	:after company
	:ensure t
	:config
	(setq company-lsp-cache-candidates t))

(use-package lsp-ui
	:after lsp-mode
	:ensure t
	:config
	(setq lsp-ui-sideline-enable nil
				lsp-ui-doc-enable nil
				lsp-ui-flycheck-enable t
				lsp-prefer-flymake nil
				lsp-ui-imenu-enable t
				lsp-ui-sideline-ignore-duplicate t))

(provide 'init-lsp)
