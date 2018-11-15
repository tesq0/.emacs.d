;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-scroll-amount '(4 ((shift) . 4))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 5) ;; keyboard scroll one line at a time

(general-define-key
 "<C-wheel-up>" 'mikus-scroll-column-left
 "<C-wheel-down>" 'mikus-scroll-column-right)

(provide 'init-mouse)
