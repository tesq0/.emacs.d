(use-package evil-smartparens
  :ensure t
  :init
  (progn

    (defun configure-smartparens-keybindings ()
      "Setup some evil keybindings for smartparens-mode."

      (general-define-key
       :states 'normal
       :keymaps 'evil-smartparens-mode-map
       "s" nil)

      (general-define-key
       :states '(normal visual motion)
       "zj" 'sp-backward-sexp
       "z;" 'sp-forward-sexp
       "zk" 'sp-down-sexp
       "zl" 'sp-up-sexp

       "zK" 'sp-end-of-sexp
       "zL" 'sp-beginning-of-sexp

       "zJ" 'sp-backward-sexp-end
       "z:" 'sp-forward-sexp-end)

      (general-define-key
       :states 'normal
       "zst"  'sp-transpose-sexp

       "zsu" 'sp-unwrap-sexp
       "zsb" 'sp-backward-unwrap-sexp

       "zfj" 'sp-backward-slurp-sexp
       "zfJ" 'sp-backward-barf-sexp

       "zf;" 'sp-forward-slurp-sexp
       "zf:" 'sp-forward-barf-sexp

       "zgn" 'sp-add-to-previous-sexp
       "zgp" 'sp-add-to-next-sexp

       "zsw" 'sp-swap-enclosing-sexp
       "zss" 'sp-splice-sexp
       "zsd;" 'sp-splice-sexp-killing-forward
       "zsdj" 'sp-splice-sexp-killing-backward
       "zsda" 'sp-splice-sexp-killing-around

       "zsc" 'sp-convolute-sexp

       "zsj" 'sp-absorb-sexp
       "zs;" 'sp-emit-sexp

       "zsJ" 'sp-extract-before-sexp
       "zs:" 'sp-extract-after-sexp

       "zsy" 'sp-split-sexp
       "zsY" 'sp-splice-sexp)
      )

    (setq sp-show-pair-from-inside t)
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
    (add-hook 'lisp-mode-hook #'smartparens-mode)
    (smartparens-global-mode 0)
    (smartparens-strict-mode nil)
    (configure-smartparens-keybindings)

    ;; Disable single quote matching for lisp modes
    (sp-with-modes sp--lisp-modes
      ;; disable ', it's the quote character!
      (sp-local-pair "'" nil :actions nil)
      ;; also only use the pseudo-quote inside strings where it serve as
      ;; hyperlink.
      (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))
      (sp-local-pair "`" nil
		     :skip-match (lambda (ms mb me)
				   (cond
				    ((equal ms "'")
				     (or (sp--org-skip-markup ms mb me)
					 (not (sp-point-in-string-or-comment))))
				    (t (not (sp-point-in-string-or-comment)))))))
    )
  )


(provide 'init-smartparens)
