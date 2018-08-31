
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


(provide 'init-diff)
