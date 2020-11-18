(use-package org
  :ensure t
  :init
  (progn
    (defun my/org-insert-todo-heading (ARG)
      (interactive "P")
      (go-end-of-visual-line)
      (org-insert-todo-heading ARG))
    (general-define-key
     :keymaps 'org-mode-map
     "<M-S-return>" 'my/org-insert-todo-heading
     )
    (evil-define-key 'normal org-mode-map
      (kbd "M-n") 'org-move-item-down
      (kbd "M-p") 'org-move-item-up)
    (setq org-todo-keywords '("TODO" "DOING" "DONE")))
  :config
  ;; add support for other src languages
  (setq org-babel-load-languages (append org-babel-load-languages '((ruby . t) (csharp . t))))
  )

(use-package htmlize
  :ensure t)

(require 'ob-csharp)




(provide 'init-org)

