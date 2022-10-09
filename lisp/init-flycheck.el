(use-package flycheck
  :commands (flycheck-mode)
  :hook (prog-mode . flycheck-mode)
  :init
  (setq flycheck-check-syntax-automatically '(save))
  :config
  (setq flycheck-highlighting-mode 'symbols)
  (setq flycheck-check-syntax-automatically '(save)))

(provide 'init-flycheck)
