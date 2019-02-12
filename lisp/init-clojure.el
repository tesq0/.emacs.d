(use-package cider
	:ensure t)

(use-package flycheck-clojure
	:ensure t
	:init
	(flycheck-clojure-setup))

(provide 'init-clojure)
