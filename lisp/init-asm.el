
(defun try-nasm-mode ()
  (when (string-equal "asm" (file-name-extension buffer-file-name))
    (nasm-mode)))

(use-package nasm-mode
  :init
  (add-hook 'find-file-hook #'try-nasm-mode))

(use-package x86-lookup
  
  :init
  (setq x86-lookup-pdf "/home/mikus/Documents/PDF/325383-sdm-vol-2abcd.pdf"))


(provide 'init-asm)
