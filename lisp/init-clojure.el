(use-package cider
  :commands (cider-mode)
  :mode (("\\.clj\\'". cider-mode))
  :config
  (setq
   clojure-indent-style 'always-align
   cider-show-error-buffer 'except-in-repl)
  (general-define-key
   :states '(normal insert)
   :keymaps 'cider-repl-mode-map
   "C-p" 'cider-repl-previous-input
   "C-n" 'cider-repl-next-input))

(use-package flycheck-clojure
  :commands (flycheck-clojure-setup)
  :hook (cider-mode . flycheck-clojure-setup))

(provide 'init-clojure)
