;; magit package

(defun initMagit ()
	(setq evil-magit-want-horizontal-movement t)

	(general-define-key
	 :keymaps 'magit-status-mode-map
	 "j" nil)

	(define-prefix-command 'mikus-magit-map)
	(define-prefix-command 'mikus-magit-blame-map)

	(defun setup-default-blame-style (type)
		(setq-local magit-blame--style (cadr magit-blame-styles)))

	(advice-add 'magit-blame--pre-blame-setup :before #'setup-default-blame-style)

	(general-define-key
	 :keymaps 'mikus-magit-blame-map
	 "C-S-c" 'magit-blame-copy-hash
	 "b" 'magit-blame
	 "B" 'magit-blame-popup
	 "n" 'magit-blame-next-chunk
	 "N" 'magit-blame-next-chunk-same-commit
	 "p" 'magit-blame-previous-chunk
	 "P" 'magit-blame-previous-chunk-same-commit
	 "r" 'magit-blame-removal
	 "f" 'magit-blame-reverse
	 "s" 'magit-blame-cycle-style
	 "c" 'magit-blame-show-commit
	 "q" 'magit-blame-quit)

	(general-define-key
	 :keymaps 'transient-map
	 "<escape>" 'transient-quit-one)

	(general-define-key
	 :keymaps 'mikus-magit-map
	 "s" 'magit-status
	 "e" 'magit-ediff
	 "d" 'magit-diff
	 "a" 'vc-annotate
	 "f" 'magit-find-file
	 "c" 'vc-find-conflicted-file
	 "b" 'magit-blame))

(use-package evil-magit
	:after magit
	:demand t
	:ensure nil
	:quelpa (evil-magit :fetcher github :repo "tesq0/evil-magit"))

(use-package magit
	:ensure t
	:init (initMagit))

(use-package smerge-mode
	:after hydra
	:ensure nil
	:config
	(defhydra unpackaged/smerge-hydra
		(:color pink :hint nil :post (smerge-auto-leave))
		"
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
		("n" smerge-next)
		("p" smerge-prev)
		("b" smerge-keep-base)
		("u" smerge-keep-upper)
		("l" smerge-keep-lower)
		("m" smerge-keep-upper)
		("o" smerge-keep-lower)
		("a" smerge-keep-all)
		("RET" smerge-keep-current)
		("\C-m" smerge-keep-current)
		("<" smerge-diff-base-upper)
		("=" smerge-diff-upper-lower)
		(">" smerge-diff-base-lower)
		("R" smerge-refine)
		("E" smerge-ediff)
		("C" smerge-combine-with-next)
		("r" smerge-resolve)
		("k" smerge-kill-current)
		("C-x C-s" (lambda ()
						(interactive)
						(save-buffer)
						(rg-project-merge-conflicts))
		 "Save, and find next conflict" :color blue)
		("q" nil "cancel" :color blue))
	;; (remove-hook 'magit-diff-visit-file-hook (car magit-diff-visit-file-hook))
	;; (remove-hook 'smerge-mode-hook (car smerge-mode-hook))
	;; :hook (((magit-diff-visit-file smerge-mode). (lambda ()
	;;                                  (when smerge-mode
	;;                                    (unpackaged/smerge-hydra/body)))))
	:init
	(general-define-key
			:keymaps 'smerge-mode-map
			"C-c m" 'unpackaged/smerge-hydra/body))


(use-package git-gutter
	:ensure t
	:init
	(progn
		(define-prefix-command 'mikus-gitgutter-map)
		(general-define-key
		 :keymaps 'mikus-gitgutter-map
		 "n" 'git-gutter:next-hunk
		 "p" 'git-gutter:previous-hunk
		 "u" 'git-gutter:revert-hunk
		 "s" 'git-gutter:stage-hunk
		 "d" 'git-gutter:popup-diff)
		(general-define-key
		 :keymaps 'mikus-magit-map
		 "g" 'mikus-gitgutter-map)
		(add-hook 'prog-mode-hook #'git-gutter-mode)))

(provide 'init-magit)
