


;; FONT
(cond
 (sys/win32p 
	(set-face-attribute 'default nil
											:family "Consolas" :height 110))
 (sys/linuxp
	(set-face-attribute 'default nil
											:family "Hack" :height 120))
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

;; initial frame size

(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 200))

(defun fix-mouse-color (frame)
	"Fix mouse pointer color after creating a FRAME to be white."
	(modify-frame-parameters frame
			   (list (cons 'mouse-color
				       (or "white"
					   (cdr (assq 'mouse-color
												(frame-parameters))))))))

(add-to-list 'after-make-frame-functions #'fix-mouse-color)


(provide 'init-ui)
