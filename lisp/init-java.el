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
  (setq-local company-manual-completion-fn #'company-lsp))

(defun init-Java ()
  "Some setup after lsp-java init."
  (advice-add 'lsp-java-boot--find-tools-jar :around #'lsp-java-boot--find-tools-jar-nixos)
  ;; (advice-remove 'lsp-java-boot--find-tools-jar #'lsp-java-boot--find-tools-jar-nixos)
  (setq lsp-java-format-enabled nil))

(add-hook 'java-mode-hook 'lsp)

(use-package groovy-mode
  :ensure t)

(provide 'init-java)
