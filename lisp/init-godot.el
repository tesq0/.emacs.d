(defun setup-gdscript ()
  (lsp)
  (setq-local company-manual-completion-fn #'company-lsp)
  (setq-local company-backends '(company-files (company-dabbrev-code :with company-lsp company-yasnippet company-keywords))))

(use-package gdscript-mode
  :demand t
  :hook (gdscript-mode-hook . setup-gdscript))

(provide 'init-godot)
