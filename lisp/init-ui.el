


;; FONT

(set-face-attribute 'default nil
                    :family "Consolas" :height 115)

(setq-default display-line-numbers nil
							display-line-numbers-widen nil)


(defun mikus:relative ()
	(setq-local display-line-numbers 'visual))
(defun mikus:absolute ()
	(setq-local display-line-numbers t))

;; colortheme

(use-package color-theme-sanityinc-tomorrow
	:ensure t
	:config
	(color-theme-sanityinc-tomorrow-night)
	)


;; initial frame size

(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 200))





(provide 'init-ui)
