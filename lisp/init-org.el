(use-package org
  :config
  (require 'ob-csharp)
  (defun my/org-insert-todo-heading (ARG)
    (interactive "P")
    (go-end-of-visual-line)
    (org-insert-todo-heading ARG))
  (general-define-key
   :keymaps 'org-mode-map
   "<M-S-return>" 'my/org-insert-todo-heading
   "<S-return>" 'org-insert-item)
  (setq org-file-apps '(("pdf". system)
			(remote . emacs)
			(auto-mode . emacs)
			(directory . emacs)
			(system . "setsid -w xdg-open %s")
			(t . system)))
  (evil-define-key 'normal org-mode-map
    (kbd "M-n") 'org-move-item-down
    (kbd "M-p") 'org-move-item-up)
  (setq org-todo-keywords '("TODO" "DOING" "DONE"))
  (setq org-babel-load-languages (append org-babel-load-languages '((ruby . t) (csharp . t)))))

(use-package htmlize)

(provide 'init-org)
