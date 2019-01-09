(defun init-Java ()

	;; lsp setup
	(setq-local company-manual-completion-fn #'company-lsp)
	(setq lsp-inhibit-message t)
	(setq lsp-eldoc-render-all t)
	(setq lsp-highlight-symbol-at-point nil)

	;; rest
	(flycheck-mode))

(use-package lsp-mode
  :ensure t)

(use-package treemacs
  :ensure t)

(use-package groovy-mode
	:ensure t)

(use-package company-lsp
  :after  company
  :ensure t
  :config
  (setq company-lsp-cache-candidates t
        company-lsp-async t))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-update-mode 'point))

(use-package lsp-java
  :ensure t
  :init
	(add-hook 'java-mode-hook 'lsp-java-enable)
	(add-hook 'java-mode-hook 'init-Java))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(provide 'init-java)
