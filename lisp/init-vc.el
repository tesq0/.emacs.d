
(with-eval-after-load 'vc
  (fullframe vc-annotate quit-window)

  (general-define-key
   :keymaps 'vc-annotate-mode-map
   :states '(normal motion)
   "q" 'quit-window
   "=" 'vc-annotate-show-diff-revision-at-line
   "C-n" 'vc-annotate-next-revision
   "C-p" 'vc-annotate-prev-revision
   "L" 'vc-annotate-show-log-revision-at-line
   )

  (general-define-key
   :keymaps 'vc-log-mode-map
   "q" 'quit-window)

  (general-define-key
   :keymaps 'vc-annotate-mode-map
   "C-c f" 'vc-annotate-find-revision-at-line
   "C-c l" 'vc-annotate-show-log-revision-at-line
   "C-c v" 'vc-annotate-toggle-annotation-visibility
   "C-c w" 'vc-annotate-working-revision
   "C-c r" 'revert-buffer
   "C-c D" 'vc-annotate-show-changeset-diff-revision-at-line
   "C-c d" 'vc-annotate-show-diff-revision-at-line
   )

  (defun center-scroll-advice (&rest ignore)
    (evil-scroll-line-to-center (line-number-at-pos)))

  (dolist (fn '(vc-annotate-prev-revision vc-annotate-next-revision vc-annotate))
    (advice-add fn :after 'center-scroll-advice)))

(provide 'init-vc)
