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
 "<normal-state> <" 'dired-up-directory
 "<normal-state> >" 'dired-find-file
 )

;; allow to change permissions
(setq wdired-allow-to-change-permissions t)

(provide 'init-dired)
