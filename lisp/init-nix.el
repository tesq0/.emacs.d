(use-package nix-mode
  :ensure t
  :init
  (defun init-nix-mode ()
    (setq-local company-backends '(company-yasnippet company-files (company-dabbrev-code company-capf company-keywords) company-dabbrev)))
  (add-hook 'nix-mode-hook 'init-nix-mode)
  (setq nix-indent-function 'nix-indent-line)
  )


(provide 'init-nix)
