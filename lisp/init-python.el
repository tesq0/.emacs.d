(use-package python-mode
  :commands python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter (("python"  . python-mode)
		("python3" . python-mode))
  :preface
  (defun setup-py ()
    (require 'lsp-pyls)
    (yas-minor-mode-on)
    (lsp-deferred)
    (add-hook 'before-save-hook #'lsp-format-buffer nil t))
  
  :hook ((python-mode . setup-py)))

(provide 'init-python)
