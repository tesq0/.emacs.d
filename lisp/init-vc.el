(with-eval-after-load 'vc
  (fullframe vc-annotate quit-window)

  (define-key vc-annotate-mode-map "q" 'quit-window)
  (define-key vc-annotate-mode-map "=" 'vc-annotate-show-diff-revision-at-line)
  (define-key vc-annotate-mode-map "C-n" 'vc-annotate-next-revision)
  (define-key vc-annotate-mode-map "C-p" 'vc-annotate-prev-revision)
  (define-key vc-annotate-mode-map "L" 'vc-annotate-show-log-revision-at-line)
  (define-key vc-annotate-mode-map "C-c f" 'vc-annotate-find-revision-at-line)
  (define-key vc-annotate-mode-map "C-c l" 'vc-annotate-show-log-revision-at-line)
  (define-key vc-annotate-mode-map "C-c v" 'vc-annotate-toggle-annotation-visibility)
  (define-key vc-annotate-mode-map "C-c w" 'vc-annotate-working-revision)
  (define-key vc-annotate-mode-map "C-c r" 'revert-buffer)
  (define-key vc-annotate-mode-map "C-c D" 'vc-annotate-show-changeset-diff-revision-at-line)
  (define-key vc-annotate-mode-map "C-c d" 'vc-annotate-show-diff-revision-at-line)

  (define-key 'vc-log-mode-map "q" 'quit-window)

  (dolist (fn '(vc-annotate-prev-revision vc-annotate-next-revision vc-annotate))
    (advice-add fn :after 'center-scroll-advice)))

(provide 'init-vc)
