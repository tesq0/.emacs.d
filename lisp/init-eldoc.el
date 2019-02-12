;; Feature `eldoc' provides a minor mode (enabled by default in Emacs
;; 25) which allows function signatures or other metadata to be
;; displayed in the echo area.

;;; Code

(use-package eldoc
  :demand t
	:init
	(progn

		(defun eldoc-print ()
			"command to trigger eldoc, add that to 'eldoc-message-commands"
			(interactive))
		(eldoc-add-command 'eldoc-print)
		(mikus-leader
			:states '(normal motion visual)
			:keymaps 'override
			"h" 'eldoc-print
			)
		)
  :config
  (setq eldoc-echo-area-use-multiline-p t))



(provide 'init-eldoc)
