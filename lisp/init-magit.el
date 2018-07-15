
;; magit package

(use-package magit
	:ensure t
	:config
	(define-key magit-mode-map (kbd "<escape>") 'magit-mode-bury-buffer)
	(define-key magit-mode-map (kbd "C-g") 'magit-mode-bury-buffer)
	;; SMERGE
	(define-key smerge-mode-map (kbd "C-c m") (lookup-key smerge-mode-map (kbd "C-c ^")))
	)

(provide 'init-magit)
