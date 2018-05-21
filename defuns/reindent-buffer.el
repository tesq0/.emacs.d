(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f4] 'indent-buffer)


(provide 'reindent-buffer)
