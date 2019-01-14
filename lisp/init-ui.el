


;; FONT
(cond
 (sys/win32p 
	(set-face-attribute 'default nil
											:family "Consolas" :height 110))
 (sys/linuxp
	(set-face-attribute 'default nil
											:family "Hack" :height 110))
 (sys/macp
	(set-face-attribute 'default nil
											:family "Hack" :height 140)))

(setq-default display-line-numbers nil
							display-line-numbers-widen nil)


(defun mikus:relative ()
	(setq-local display-line-numbers 'visual))
(defun mikus:absolute ()
	(setq-local display-line-numbers t))

(use-package doom-themes
	:ensure t
	:init
	(progn
		(doom-themes-org-config)
		(add-hook 'after-init-hook (lambda () (load-theme 'doom-molokai t)))
		))


;; colortheme

;; (use-package color-theme-sanityinc-tomorrow
;; 	:ensure t
;; 	:config
;; 	(color-theme-sanityinc-tomorrow-eighties)
;; 	)




;; initial frame size

(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 200))





(provide 'init-ui)
