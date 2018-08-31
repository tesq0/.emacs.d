(use-package org
	:ensure t
	:init
	(progn
		(evil-define-key 'normal org-mode-map
		 (kbd "M-n") 'org-move-item-down
		 (kbd "M-p") 'org-move-item-up)

		;; add support for other src languages
		(setq org-babel-load-languages (append org-babel-load-languages '((ruby . t) (csharp . t))))
		)
	)

(use-package ob-csharp
	:ensure t)

(use-package htmlize
	:ensure t)

(require 'ob-csharp)

	


(provide 'init-org)

