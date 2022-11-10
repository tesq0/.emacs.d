(with-eval-after-load 'org
  (defun my/org-insert-todo-heading (ARG)
    (interactive "P")
    (go-end-of-visual-line)
    (org-insert-todo-heading ARG))
  
  (setq org-file-apps '(("pdf". system)
			(remote . emacs)
			(auto-mode . emacs)
			(directory . emacs)
			(system . "setsid -w xdg-open %s")
			(t . system)))
 
  (setq org-todo-keywords '("TODO" "DOING" "DONE"))
  (setq org-babel-load-languages (append org-babel-load-languages '((ruby . t) (csharp . t)))))

(provide 'init-org)
