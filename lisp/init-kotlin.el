(use-package kotlin-mode
  :ensure t)

(use-package flycheck-kotlin
  :after kotlin-mode
  :ensure t
  :init
  (flycheck-kotlin-setup))

(provide 'init-kotlin)
