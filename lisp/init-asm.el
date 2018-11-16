
(defun try-nasm-mode ()
	(when (string-equal "asm" (file-name-extension buffer-file-name))
		(nasm-mode)))

(use-package nasm-mode
	:init
	(add-hook 'find-file-hook #'try-nasm-mode))

(provide 'init-asm)
