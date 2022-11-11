;; FONT
(cond
 (sys/win32p 
  (set-face-attribute 'default nil
		      :family "Consolas" :height 160))
 (sys/linuxp
  (set-face-attribute 'default nil
		      :family "Hack" :height 120))
 (sys/macp
  (set-face-attribute 'default nil
		      :family "Hack" :height 140)))

(setq-default display-line-numbers nil
	      display-line-numbers-widen nil)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(require 'ansi-color)

(provide 'init-ui)
