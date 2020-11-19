(use-package lsp-mode
  :ensure t
  ;; :quelpa
  ;; (lsp-mode :fetcher github :repo "emacs-lsp/lsp-mode")
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

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :init
  (setq dap-auto-configure-features '(sessions locals controls))
  (dap-mode t)
  (dap-auto-configure-mode t)
  (dap-tooltip-mode -1)
  (custom-set-faces
   '(dap-ui-pending-breakpoint-face ((t (:underline "dim gray"))))
   '(dap-ui-verified-breakpoint-face ((t (:underline "green")))))

  (add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra))))


(provide 'init-lsp)
