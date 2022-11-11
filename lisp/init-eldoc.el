;; Feature `eldoc' provides a minor mode (enabled by default in Emacs
;; 25) which allows function signatures or other metadata to be
;; displayed in the echo area.

;;; Code
(autoload 'eldoc "eldoc-mode")
(add-hook 'prog-mode-hook 'eldoc-mode)

(with-eval-after-load 'eldoc
  (setq eldoc-echo-area-use-multiline-p t))

(provide 'init-eldoc)
