(use-package wgrep
  :ensure t)

(use-package wgrep-helm
  :after wgrep
  :ensure t
  :init
  (setq wgrep-enable-key "\C-c g"
	wgrep-auto-save-buffer t)
  )

(provide 'init-wgrep)
