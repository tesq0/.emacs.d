;; jump to definition
(use-package dumb-jump
  :init
  (progn
    (add-hook 'dumb-jump-after-jump-hook 'my/hl-line)
    (setq dumb-jump-prefer-searcher 'rg)
    ))

(use-package smart-jump
  :init
  (smart-jump-setup-default-registers))


(provide 'init-jump-to-def)
