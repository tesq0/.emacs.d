(use-package org
	:ensure t
	:init
	(progn
		(evil-define-key 'normal org-mode-map
		 (kbd "M-n") 'org-move-item-down
		 (kbd "M-p") 'org-move-item-up)
		)
	)

(provide 'init-org)

