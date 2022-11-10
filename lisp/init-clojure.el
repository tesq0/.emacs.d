(autoload "cider-mode" 'cider)

(with-eval-after-load 'cider
  (setq
   clojure-indent-style 'always-align
   cider-show-error-buffer 'except-in-repl))

(provide 'init-clojure)
