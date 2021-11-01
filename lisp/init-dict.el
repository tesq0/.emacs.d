(use-package google-translate
  
  :init
  (progn
    (setq
     google-translate-default-source-language "English"
     google-translate-default-target-language "Polish")))
(use-package dictionary
  
  :init
  (progn
    (define-prefix-command 'dict-map)
    (general-define-key
     :keymaps 'dict-map
     "d" 'dictionary-search
     "g" 'google-translate-smooth-translate)
    (mikus-leader "d" 'dict-map)
    )
  :config
  (set-face-foreground 'dictionary-reference-face "CornflowerBlue")
  )

(setq ispell-program-name "aspell"
      ispell-library-directory "~/.nix-profile/lib/aspell"
      ispell-cmd-args "--conf ~/.config/aspell.conf")

(provide 'init-dict)
