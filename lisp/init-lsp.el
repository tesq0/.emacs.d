(use-package lsp-mode
  :commands lsp boot-lsp
  :init
  (setf n nil)
  (setq lsp-keymap-prefix "C-c l")
  (defun boot-lsp ()
    (require 'lsp-diagnostics)
    (require 'lsp-modeline)
    (lsp)
    (setq company-backends (default-value 'company-backends))
    (setq-local company-manual-completion-fn 'company-capf)
    (flycheck-mode nil))
  :hook ((web-mode . (lambda ()
		       (when-file-extension-matches '("[jt]sx" "svelte" "[sl]?css") 'boot-lsp)))
	 (typescript-mode . boot-lsp)
	 (js2-mode . boot-lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-enable-indentation nil
	lsp-enable-xref t
	lsp-inhibit-message t
	lsp-headerline-breadcrumb-enable nil
	lsp-eldoc-render-all n
	lsp-enable-file-watchers nil
	lsp-highlight-symbol-at-point nil)
  (add-to-list 'lsp-language-id-configuration '(".*\\.scss" . "scss"))
  )

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol
  :after lsp-mode
  :init
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

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
