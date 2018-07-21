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
  "windows"
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
  ("v" split-window-right)
  ("x" split-window-below)
  ;("t" transpose-frame "'")
  ;; winner-mode must be enabled
  ("u" winner-undo)
  ("m" ace-move-window)
  ("r" winner-redo) ;;Fixme, not working?
  ("o" delete-other-windows :exit t)
  ("a" ace-window)
  ("s" ace-swap-window :exit t)
  ("da" ace-delete-window)
  ("dw" delete-window)
  ("db" kill-this-buffer)
	)

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
