(use-package org
	:ensure t
	:init
	(progn
		(evil-define-key 'normal org-mode-map
		 (kbd "M-n") 'org-move-item-down
		 (kbd "M-p") 'org-move-item-up)
		)
	:config
	;; add support for other src languages
	(setq org-babel-load-languages (append org-babel-load-languages '((ruby . t) (csharp . t))))
	)

(use-package htmlize
	:ensure t)

(require 'ob-csharp)

	


(provide 'init-org)

