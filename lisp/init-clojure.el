(autoload 'clojure-mode "clojure-mode")

(add-to-list 'auto-mode-alist '("\\.clj\\'". clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn\\'". clojure-mode))

(with-eval-after-load 'cider
  (setq
   clojure-indent-style 'always-align
   cider-show-error-buffer 'except-in-repl))

(add-hook 'clojure-mode-hook (lambda ()
			       (require 'cider)))

(provide 'init-clojure)
