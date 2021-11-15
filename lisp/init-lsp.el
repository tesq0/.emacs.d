(use-package lsp-mode
  
  :config
  (require 'dash-functional)
  (setq lsp-enable-indentation nil
	lsp-inhibit-message t
	lsp-eldoc-render-all n
	lsp-enable-file-watchers nil
	lsp-highlight-symbol-at-point nil))

(use-package company-lsp
  :after company
  
  :config
  (setq company-lsp-cache-candidates t))

(use-package lsp-ui
  :after lsp-mode
  
  :config
  (setq lsp-ui-sideline-enable nil
	lsp-ui-doc-enable nil
	lsp-ui-flycheck-enable t
	lsp-prefer-flymake nil
	lsp-ui-imenu-enable t
	lsp-ui-sideline-ignore-duplicate t))


(provide 'init-lsp)
