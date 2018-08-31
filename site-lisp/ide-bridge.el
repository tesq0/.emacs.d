;;
;; orginal file can be found at : http://code.google.com/p/csense/

(defvar ide-bridge-port 8989
  "port used to connect to IdeBridge")

;; this should be a keymap, but I'm lazy
(defvar ide-bridge-bindings
  `()
  "Keybindings for common Code Sense tasks.")

(defvar ide-bridge-completion-symbol-beginning-position nil
  "Beginning position of the symbol currently being completed.")

(defvar ide-bridge-completion-bindings
  `((,(kbd "<down>") . ide-bridge-completion-next-line)
    (,(kbd "<up>") . ide-bridge-completion-previous-line)
    (,(kbd "<next>") . ide-bridge-completion-next-page)
    (,(kbd "<prior>") . ide-bridge-completion-previous-page)
    (,(kbd "<ESC>") . ide-bridge-completion-cancel)
    (,(kbd "C-g") . ide-bridge-completion-cleanup)
    (,(kbd "<tab>") . ide-bridge-completion-insert-selection)
    (,(kbd "<RET>") . ide-bridge-completion-insert-selection)
    (,(kbd "<SPC>") . ide-bridge-completion-insert-selection-spc))
  "Keybindings for code completion.")

(defvar ide-bridge-insight-bindings
  `((,(kbd "<down>") . ide-bridge-completion-next-line)
    (,(kbd "<up>") . ide-bridge-completion-previous-line)
    (,(kbd "<ESC>") . ide-bridge-completion-cancel)
    (,(kbd "C-g") . ide-bridge-completion-cleanup))
  "Keybindings for insight completion.")

(defvar ide-bridge-completion-editing-commands
  '(self-insert-command
    c-electric-backspace)
  "These commands can be used during completion to edit the pattern.")

(defvar ide-bridge-completion-just-started nil
  "Used to prevent the post command hook to kick in when the
  completion list is shown for the first time.")

(defvar ide-bridge-saved-keys nil
  "The original keybindings are saved here when Ide-Bridge rebinds
  some keys temporarily.")

(defun ide-bridge-process-filter (proc string)
  (message string)
  (eval (read string))
)

(defun ide-bridge-insert-candidate (string)
  (setq ide-bridge-candidate string)
  (delete-region ide-bridge-completion-symbol-beginning-position (point))
  (insert ide-bridge-candidate)
)

(defun ide-bridge-connect (process-name port)
  "Start the CSharpComplete subprocess."
  (if (not (eq (process-status process-name) 'open))
      (let ((coding-system-for-write 'mule-utf-8-dos)
            (coding-system-for-read 'mule-utf-8-dos))
        (message (concat "connecting to the " process-name))
        (condition-case nil
            (set-process-filter
             (open-network-stream process-name nil "localhost" port)
             'ide-bridge-process-filter)
          (error (concat "Unable to connect to " process-name))
          ))))

(defun ide-bridge-start-process ()
  "Start the CSharpComplete subprocess."
  (ide-bridge-connect "ide-bridge-process" ide-bridge-port))

(defun ide-bridge-stop-process ()
  "Start the CSharpComplete subprocess."
  (if (eq (process-status "ide-bridge-process") 'open)
      (progn
        (message "disconnecting from ide-bridge-process")
        (process-send-string "ide-bridge-process" "exit:")
        (delete-process "ide-bridge-process")

        )))

(defun ide-bridge-setup ()
  "Setup Code Sense for the current buffer."
  (interactive)

  (ide-bridge-start-process)

  (dolist (binding ide-bridge-bindings)
    (local-set-key (car binding) (cdr binding))))

(defun ide-bridge-test ()
  "test various stuff."
  (interactive)
  (skip-syntax-backward "w_")
)

(defun ide-bridge-self-insert-complete ()
  "self-insert 'n complete."
  (interactive)
  (self-insert-command 1)
  (if (eq (process-status "ide-bridge-process") 'open)
      (ide-bridge-complete 't))
)

(defun ide-bridge-ensure-connected ()
  "Connect to the specific instance of visual if necessary."
  (interactive)
  (if (not (eq (process-status "ide-bridge-process") 'open))
      (ide-bridge-start-process))
)

(defun ide-bridge-connected ()
  "Test if connected to the specific instance of visual."
  (interactive)
  (if (not (eq (process-status "ide-bridge-process") 'open))
      nil
    't)
)

(defun ide-bridge-go-to-definition ()
  "Go to definition of the code element at point."
  (interactive)

  (ide-bridge-ensure-connected)

  (process-send-string "ide-bridge-process" "buffer:")
  (process-send-region "ide-bridge-process" (point-min) (point-max))
  (process-send-string "ide-bridge-process" "")

  (process-send-string "ide-bridge-process"
                       (concat "go-to-definition:"
                               (buffer-file-name)
                               "|" (number-to-string (point))
                               "|" (number-to-string (line-number-at-pos))
                               "|" (number-to-string (current-column))
                               ""))
)

(defun ide-bridge-go-to ()
  "Go to."
  (interactive)

  (ide-bridge-ensure-connected)

  (process-send-string "ide-bridge-process" "buffer:")
  (process-send-region "ide-bridge-process" (point-min) (point-max))
  (process-send-string "ide-bridge-process" "")

  (process-send-string "ide-bridge-process"
                       (concat "go-to:"
                               (buffer-file-name)
                               "|" (number-to-string (point))
                               "|" (number-to-string (line-number-at-pos))
                               "|" (number-to-string (current-column))
                               ""))
)

(defun ide-bridge-set-solution ()
  "Set the current solution file."
  (interactive)

  (ide-bridge-ensure-connected)

  (process-send-string "ide-bridge-process"
                       (concat "list-solutions:"
                               (buffer-file-name)
                               ""))
)

(defun ide-bridge-build-project ()
  "Build the project containing the current buffer."
  (interactive)

  (ide-bridge-ensure-connected)

  (process-send-string "ide-bridge-process"
                       (concat "build-by-filename:"
                               (buffer-file-name)
                               ""))
)

(defun ide-bridge-rebuild-project ()
  "Build the project containing the current buffer."
  (interactive)

  (ide-bridge-ensure-connected)

  (process-send-string "ide-bridge-process"
                       (concat "rebuild-by-filename:"
                               (buffer-file-name)
                               ""))
)

(defun ide-bridge-select-solution (solutions)
  "Select a solution in solutions list."

  (let ((solution (completing-read "solution:" solutions)))
    (process-send-string "ide-bridge-process"
                         (concat "set-solution:"
                                 solution
                                 "")))
)


(defun ide-bridge-test ()
  "For test purpose."
  (interactive)

  (ide-bridge-ensure-connected)

  (process-send-string "ide-bridge-process" "buffer:")
  (process-send-region "ide-bridge-process" (point-min) (point-max)) ; don't send the filter part
  (process-send-string "ide-bridge-process" "")

  (process-send-string "ide-bridge-process"
                       (concat "test:"
                               (buffer-file-name)
                               "|" (number-to-string (point))
                               ""))
)

(defun ide-bridge-go-to-next-location ()
  "If ide-bridge-go-to-definition have more than one target location cycle through the target locations."
  (interactive)

  (ide-bridge-ensure-connected)


  (process-send-string "ide-bridge-process"
                       (concat "go-to-next-location:"
                               ""))
)

(defun ide-bridge-complete (&optional onlyContext)
  "Do completion at point."
  (interactive)

  (message "Do completion at point!!")

  (ide-bridge-ensure-connected)

  (if (and (not ide-bridge-saved-keys) ; if ide-bridge-saved-keys is set that mean we are already in a complete session
           (ide-bridge-connected))
      (progn
        (save-excursion
          (skip-syntax-backward "w_")
          (setq ide-bridge-completion-symbol-beginning-position (point)))
        (ide-bridge-show-completion-for-point onlyContext))

    (if (ide-bridge-connected) ; if we are already in a complete session
        (ide-bridge-completion-next-line)))

)
(put 'ide-bridge-complete 'ide-bridge-allowed-during-completion t)

(defun ide-bridge-show-completion-for-point (onlyContext)
  (save-excursion
    ;; (end-of-line) ;; ie ignore the rest of the current line
    (process-send-string "ide-bridge-process" "buffer:")
    (process-send-region "ide-bridge-process" (point-min) (point-max)) ; don't send the filter part
    (process-send-string "ide-bridge-process" ""))

  (process-send-string "ide-bridge-process"
                       (concat "complete:"
                               (ide-bridge-calculate-popup-position)
                               "|" (number-to-string (frame-char-height))
                               "|" (buffer-file-name)
                               "|" (number-to-string (point))
                               "|" (number-to-string (line-number-at-pos))
                               "|" (number-to-string (current-column))
                               "|" (if onlyContext "1" "0")
                               "|" (ide-bridge-completion-get-filter)
                               ""))
)

(defun ide-bridge-init-complete-context (context-bindings)
  (add-hook 'pre-command-hook 'ide-bridge-completion-pre-command)
  (add-hook 'post-command-hook 'ide-bridge-completion-post-command)

  (dolist (binding context-bindings)
    (let ((key (car binding))
          (command (cdr binding)))
      (push (cons key (lookup-key (current-local-map) key))
            ide-bridge-saved-keys)
      (define-key (current-local-map) key command)))

  (setq ide-bridge-completion-just-started t)
)

(defun ide-bridge-completion-pre-command ()
  "Guard function which terminates the completion if any other
command is used than the allowed ones."

  (unless (or (memq this-command ide-bridge-completion-editing-commands)
              (get this-command 'ide-bridge-allowed-during-completion))
   (ide-bridge-completion-cleanup))

  (if (not (ide-bridge-connected))
      (ide-bridge-completion-cleanup)))


(defun ide-bridge-completion-post-command ()
  "Guard function which updates the completion list after typing,
or terminates the completion if any other command is used than
the allowed ones."
  (if ide-bridge-completion-just-started
      (setq ide-bridge-completion-just-started nil)

    (if (or (< (point) ide-bridge-completion-symbol-beginning-position)
            (and (eq this-command 'self-insert-command)
                 (let ((syntax (char-syntax (char-before))))
                   (not (or (eq syntax ?w)
                            (eq syntax ?_))))))
        (ide-bridge-completion-cleanup)

      (if (memq this-command ide-bridge-completion-editing-commands)
          (ide-bridge-update-completion-list)))))


(defun ide-bridge-update-completion-list ()
  "Update the displayed completion list."
  (process-send-string "ide-bridge-process" (concat "filter:" (ide-bridge-completion-get-filter) ""))
)


(defun ide-bridge-completion-get-filter ()
  "Return the current filter from the source buffer for the
completion."
  (buffer-substring ide-bridge-completion-symbol-beginning-position (point)))


(defun ide-bridge-completion-cleanup ()
  "Hide the completion frame and restore keybindings."
  (interactive)

  (remove-hook 'pre-command-hook 'ide-bridge-completion-pre-command)
  (remove-hook 'post-command-hook 'ide-bridge-completion-post-command)

  (if ide-bridge-saved-keys
      (dolist (binding ide-bridge-saved-keys)
        (define-key (current-local-map) (car binding) (cdr binding))))
  (setq ide-bridge-saved-keys nil)

  (if (ide-bridge-connected)
      (process-send-string "ide-bridge-process" "hide:"))

)

(defun ide-bridge-completion-next-line ()
  "Move to next item in the completion list."
  (interactive)
  (process-send-string "ide-bridge-process" "next-line:"))

(put 'ide-bridge-completion-next-line 'ide-bridge-allowed-during-completion t)


(defun ide-bridge-completion-previous-line ()
  "Move to previous item in the completion list."
  (interactive)
  (process-send-string "ide-bridge-process" "previous-line:"))

(put 'ide-bridge-completion-previous-line 'ide-bridge-allowed-during-completion t)


(defun ide-bridge-completion-next-page ()
  "Move to next page in the completion list."
  (interactive)
  (process-send-string "ide-bridge-process" "next-page:"))

(put 'ide-bridge-completion-next-page 'ide-bridge-allowed-during-completion t)


(defun ide-bridge-completion-previous-page ()
  "Move to previous page in the completion list."
  (interactive)
  (process-send-string "ide-bridge-process" "previous-page:"))

(put 'ide-bridge-completion-previous-page 'ide-bridge-allowed-during-completion t)


(defun ide-bridge-completion-insert-selection ()
  "Insert selected item at point into the buffer."
  (interactive)
  (process-send-string "ide-bridge-process" "insert:")
  (ide-bridge-completion-cleanup)
)
(put 'ide-bridge-completion-insert-selection 'ide-bridge-allowed-during-completion t)

(defun ide-bridge-completion-insert-selection-spc ()
  "Insert selected item at point into the buffer plus a space."
  (interactive)
  (process-send-string "ide-bridge-process" "insert-spc:")
  (ide-bridge-completion-cleanup)
)
(put 'ide-bridge-completion-insert-selection-spc 'ide-bridge-allowed-during-completion t)


(defun ide-bridge-completion-cancel ()
  "Cancel completion in progress."
  ;; post command hook will take care of it
  (interactive))

(defun ide-bridge-pos-frame-parameter (param)
  (let ((pos (frame-parameter nil param)))
    (if (listp pos)
        (car (cdr pos))
      pos)))

(defun ide-bridge-calculate-popup-position ()
  "Calculate pixel position of popup at point."
  (let* ((point-pos (posn-at-point))
         (point-xy (posn-x-y point-pos))
         (edges (window-pixel-edges))
         (x (+ (car point-xy) (ide-bridge-pos-frame-parameter 'left) (car edges)))
         (y (+ (cdr point-xy) (ide-bridge-pos-frame-parameter 'top) (car (cdr edges)))))
    (concat (number-to-string x) "|" (number-to-string y))))

;(ide-bridge-setup)

(provide 'ide-bridge)
