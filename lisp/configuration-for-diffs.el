(defun update-diff-refine-colors ()
  "update the colors for diff faces"
  (set-face-attribute 'diff-refine-added nil
                      :foreground "white" :background "darkgreen")
  (set-face-attribute 'diff-refine-removed nil
                      :foreground "white" :background "darkred")
  (set-face-attribute 'diff-refine-removed nil
                      :foreground "white" :background "darkred")
  (set-face-attribute 'diff-refine-changed nil
                      :foreground "white" :background "darkblue"))

(eval-after-load "diff-mode"
  '(update-diff-refine-colors))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-diff-options "-w")
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)
