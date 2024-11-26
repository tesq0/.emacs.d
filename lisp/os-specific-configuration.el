(when (memq window-system '(mac ns x))
   (setq mac-option-modifier 'alt
	 mac-command-modifier 'meta)
   ;; (load "exec-path-from-shell")
   ;; (exec-path-from-shell-initialize)
   )
