(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (general-define-key
     :keymaps 'ac-completing-map
     "C-n" 'ac-next
     "C-p" 'ac-previous
     )
    (general-define-key
     "C-SPC" 'auto-complete)
    ;; (init-ac-company)
    )
  :config
  (global-auto-complete-mode)
  
  )

(defun init-ac-company ()
  "Init autocomplete company bridge."
  (require 'ac-company)
  (ac-company-define-source ac-source-company-omnisharp company-omnisharp)
  )



(provide 'init-autocomplete)
