(autoload 'yas-minor-mode "yasnippet")

(add-hook 'prog-mode-hook 'yas-minor-mode)

(with-eval-after-load 'yasnippet
  (define-prefix-command 'my-snippet-map)
  (define-key 'my-snippet-map "i" 'yas-insert-snippet)
  (define-key 'my-snippet-map "v" 'yas-visit-snippet-file)
  (define-key 'my-snippet-map "n" 'yas-new-snippet)
  
  ;; Load custom snippets
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  
  (yas-reload-all)

  (define-key yas-minor-mode-map (kbd "C-c i") my-snippet-map))

(provide 'init-yasnippet)
