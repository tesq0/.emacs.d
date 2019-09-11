(use-package cider
	:ensure t
	:init
	(progn
		(setq
		 clojure-indent-style 'always-align
		 cider-show-error-buffer 'except-in-repl)
		(general-define-key
		 :states '(normal insert)
		 :keymaps 'cider-repl-mode-map
		 "C-p" 'cider-repl-previous-input
		 "C-n" 'cider-repl-next-input)))

(use-package flycheck-clojure
	:ensure t
	:init
	(flycheck-clojure-setup))

(provide 'init-clojure)
