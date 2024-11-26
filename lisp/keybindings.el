(global-set-key (kbd "RET") 'newline-and-indent)

;; override this fucking shit ESC
(define-key ctl-x-map (kbd "<ESC>" ) nil)

(define-prefix-command 'toggle-map)

(define-key toggle-map (kbd "l") 'linum-mode)
(define-key toggle-map (kbd "t") 'toggle-truncate-lines)
(define-key toggle-map (kbd "d") 'toggle-debug-on-error)

(global-set-key (kbd "C-c t") 'toggle-map)

(define-prefix-command	'fast-ex-map)
(global-set-key (kbd "C-c x") 'fast-ex-map)
(define-key fast-ex-map (kbd "e") 'aweshell-new)
(define-key fast-ex-map (kbd "f") 'explorer)
(define-key fast-ex-map (kbd "p") 'power-shell)
(define-key fast-ex-map (kbd "t") 'terminal)

(define-key ctl-x-5-map (kbd "n") 'make-frame)
(define-key ctl-x-5-map (kbd "b") 'switch-to-buffer-other-frame)
(define-key ctl-x-5-map (kbd "o") 'delete-other-frames)
(define-key ctl-x-5-map (kbd "c") 'delete-frame)
(define-key ctl-x-5-map (kbd "f") 'find-file)
(define-key ctl-x-5-map (kbd "C-i") 'other-frame)
(global-set-key (kbd "C-c C-e") 'eval-buffer)
(global-set-key (kbd "C-h h") nil) ;; disable that shitty hello file
(define-key ctl-x-map (kbd "C-h") 'help-command)

(global-set-key (kbd "<C-escape>") 'keyboard-quit)
(global-set-key (kbd "M-n") 'duplicate-line)

(define-prefix-command	'fast-buffer-map)

(define-prefix-command	'convert-case-map)
(global-set-key (kbd "C-c c") 'convert-case-map)
(define-key convert-case-map (kbd "b") 'camel-to-burger-case)


(define-prefix-command	'open-map)
(global-set-key (kbd "C-c o") 'open-map)

(global-set-key (kbd "<C-tab>") 'switch-to-the-window-that-displays-the-most-recently-selected-buffer)

(define-prefix-command	'insert-stuff-map)
(define-key insert-stuff-map (kbd "b") 'insert-buffer-basename)
(global-set-key (kbd "C-c i") 'insert-stuff-map)


(global-set-key (kbd "<f8>") 'compile)
