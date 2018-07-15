(use-package dired-quick-sort
  :ensure t
  :config
  (dired-quick-sort-setup))

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(general-define-key
 :keymaps 'dired-mode-map
 "<normal-state> ;" 'evil-forward-char
 )

;; allow to change permissions
(setq wdired-allow-to-change-permissions t)

(provide 'init-dired)
