(use-package nix-mode
  :ensure t
  :init
  (setq nix-indent-function 'nix-indent-line))

(defun fix-nix-options ()
  (interactive)

  ;; (setq nixos-options-json-file
  ;; 			(let* ((cmd
  ;; 							"export NIXPKGS_ALLOW_UNFREE=1; nix-build -Q --no-out-link '<nixpkgs/nixos/release.nix>' -A options 2>/dev/null")
  ;; 						 (dir (replace-regexp-in-string "\n\\'" ""
  ;; 																						(shell-command-to-string cmd))))
  ;; 				(expand-file-name "share/doc/nixos/options.json" dir)))

  ;; (setq nixos-options
  ;; 			(if (file-exists-p nixos-options-json-file)
  ;; 					(let* ((json-key-type 'string)
  ;; 								 (raw-options (json-read-file nixos-options-json-file)))
  ;; 						(mapcar 'nixos-options--make-alist raw-options))
  ;; 				(message "Warning: Cannot find nixos option file.")))
  )

(defun init-nix-mode ()
  (setq-local company-backends '(company-yasnippet company-files (company-nixos-options company-dabbrev-code company-capf company-keywords) company-dabbrev)))

(use-package company-nixos-options
  :ensure t
  :init
  (add-hook 'nix-mode-hook 'init-nix-mode))

(provide 'init-nix)
