;; magit package

(defun initMagit ()
  (setq evil-magit-want-horizontal-movement t)

  (defun setup-default-blame-style (type)
    (setq-local magit-blame--style (cadr magit-blame-styles)))

  (advice-add 'magit-blame--pre-blame-setup :before #'setup-default-blame-style))

(provide 'init-magit)
