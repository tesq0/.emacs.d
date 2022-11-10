;; Feature `eldoc' provides a minor mode (enabled by default in Emacs
;; 25) which allows function signatures or other metadata to be
;; displayed in the echo area.

;;; Code
(autoload 'eldoc "eldoc-mode")
(add-hook 'prog-mode-hook 'eldoc-mode)

(with-eval-after-load 'eldoc
  (global-eldoc-mode 0)
  (defun turn-on-eldoc-mode ())
  (defun eldoc-print ()
    "command to trigger eldoc, add that to 'eldoc-message-commands"
    (interactive)
    (eldoc-print-current-symbol-info))
  (eldoc-add-command 'eldoc-print)
  (setq eldoc-documentation-function
	(lambda ()
	  (when (eql last-command-event 32)
	    (let (eldoc-documentation-function)
	      (eldoc-print-current-symbol-info)))))
  (setq eldoc-echo-area-use-multiline-p t))

(provide 'init-eldoc)
