(defun lsp-java-boot--find-tools-jar-nixos (orig-fun &rest args)
	"Find the java tools.jar on nixos."
	(let* ((command "nix-env -q --out-path openjdk | awk '{print $2}'")
				 (raw-output (shell-command-to-string command))
				 (command-output (s-chomp raw-output))
				 (full-path (concat command-output "/lib/openjdk/lib/tools.jar"))
				 (lsp-java-boot-java-tools-jar full-path))
		(message "java jar %s" lsp-java-boot-java-tools-jar)
		(apply orig-fun)))

(defun setup-Java ()
	"Setup before lsp-java mode."
	(setq-local company-manual-completion-fn #'company-lsp)
	(flycheck-mode))

(defun init-Java ()
	"Some setup after lsp-java init."
	(advice-add 'lsp-java-boot--find-tools-jar :around #'lsp-java-boot--find-tools-jar-nixos)
	;; (advice-remove 'lsp-java-boot--find-tools-jar #'lsp-java-boot--find-tools-jar-nixos)
	(setq lsp-inhibit-message t
				lsp-eldoc-render-all t
				lsp-highlight-symbol-at-point nil))

(use-package lsp-mode
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
	:demand t
  :init
	(init-Java)
	(add-hook 'java-mode-hook 'lsp)
	(add-hook 'java-mode-hook 'setup-Java))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(provide 'init-java)
