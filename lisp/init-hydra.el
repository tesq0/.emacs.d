(use-package ace-window
	:ensure t
	:defer 1
	:config 
	(setq aw-keys '(?a ?s ?d ?f ?g ?j ?k ?l ?\;))
	)

(use-package hydra
	:ensure t

	)


(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defhydra hydra-window (:color red
                        :hint nil)
  "
Window operations:
--------------------------------------------------------------------------------------
_j_: splitter left    		 _J_ move left          _b_ balance          _u_ winner undo     _m_ ace move
_;_: splitter right        _:_ move right         _-_ text decrease    _r_ winner redo     _s_ ace swap
_l_: splitter up           _L_ move top           _=_ text increase    _|_ split right     _da_ ace delete
_k_: splitter down         _K_ move bottom        _o_ delete other		 ___ split down      _a_ ace

"
	("j" ( hydra-move-splitter-left 10 ))
  ("k" ( hydra-move-splitter-down 10 ))
  ("l" ( hydra-move-splitter-up 10 ))
  (";" ( hydra-move-splitter-right 10 )) 
	("J" evil-window-move-far-left)
	(":" evil-window-move-far-right)
	("K" evil-window-move-very-bottom)
	("L" evil-window-move-very-top)
	("b" balance-windows) 
  ("-" text-scale-decrease)
  ("=" text-scale-increase)
  ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)) :exit t)
  ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)) :exit t)
  ;; winner-mode must be enabled
  ("u" winner-undo)
  ("m" ace-move-window)
  ("r" winner-redo) ;;Fixme, not working?
  ("o" delete-other-windows :exit t )
  ("a" ace-window)
  ("s" ace-swap-window :exit t )
  ("da" ace-delete-window))

(defhydra hydra-fzf (:color red
                        :hint nil)
	"Hydra fzf 
r - root
h - home
"
	("r"  (helm-fzf "/") :exit t)
  ("h"  (helm-fzf "~/") :exit t)
	
	)



(provide 'init-hydra)
