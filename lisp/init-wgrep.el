(use-package wgrep
  )

(use-package wgrep-helm
  :after wgrep
  :init
  (setq wgrep-enable-key "\C-c g"
	wgrep-auto-save-buffer t)
  )

(provide 'init-wgrep)
