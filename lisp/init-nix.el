(use-package nix-mode
  :init
  (defun init-nix-mode ()
    (setq-local company-backends '(company-yasnippet company-files (company-dabbrev-code company-capf company-keywords) company-dabbrev)))
  (add-hook 'nix-mode-hook 'init-nix-mode)
  (setq nix-indent-function 'nix-indent-line)

  )

(with-eval-after-load 'lsp
  (progn
    (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
		      :major-modes '(nix-mode)
		      :server-id 'nix))
    )
  )


(provide 'init-nix)
