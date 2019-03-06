;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-scroll-amount '(4 ((shift) . 4))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 5) ;; keyboard scroll one line at a time

(general-define-key
 "<C-mouse-4>" 'mikus-scroll-column-left
 "<C-mouse-5>" 'mikus-scroll-column-right
 "<C-S-mouse-4>" 'text-scale-increase
 "<C-S-mouse-5>" 'text-scale-decrease)

(provide 'init-mouse)
