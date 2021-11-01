(use-package kotlin-mode
  )

(use-package flycheck-kotlin
  :after kotlin-mode
  
  :init
  (flycheck-kotlin-setup))

(provide 'init-kotlin)
