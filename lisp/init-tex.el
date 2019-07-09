(use-package auctex
	:ensure t
	:init
	(progn

		(setq tex-command "pdftex")
		(setq TeX-view-program-list '(("zathura" ("zathura %o"))))
		(setq TeX-view-program-selection '(((output-dvi has-no-display-manager) "dvi2tty") ((output-dvi
	style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf
	"zathura") (output-html "xdg-open")))

		(defun TeX-view ()
			(interactive)
			(tex-send-command "llpp" (tex-append tex-print-file ".pdf")))

		(defun LaTeX-save-and-compile ()
			"Save and compile the tex project using latexmk.
If compilation fails, split the current window and open error-buffer
then jump to the error line, if errors corrected, close the error-buffer
window and close the *TeX help* buffer."
			(interactive)
			(progn
				;; ;; turn off smartparens because LaTeX-electric-left-right-brace
				;; ;; offers more for specific LaTeX mode
				;; ;; Since SP is always triggered later by sth., so put these two lines here
				;; (turn-off-smartparens-mode)
				;; (setq LaTeX-electric-left-right-brace t)
				(let ((TeX-save-query nil)
							(TeX-process-asynchronous nil)
							(master-file (TeX-master-file)))
					(TeX-save-document "")
					;; clean all generated files before compile
					;; DO NOT do it when up-to-date, remove this line in proper time
					(let ((TeX-clean-confirm nil))
						(TeX-clean t))
					(TeX-run-TeX "latexmk"
											 (TeX-command-expand "latexmk -pdflatex='pdflatex -file-line-error -synctex=1' -pdf %s" 'TeX-master-file)
											 master-file)
					(if (plist-get TeX-error-report-switches (intern master-file))
							;; avoid creating multiple windows to show the *TeX Help* error buffer
							(if (get-buffer-window (get-buffer "*TeX Help*"))
									(TeX-next-error)
								(progn
									(split-window-vertically -10)
									(TeX-next-error)))
						;; if no errors, delete *TeX Help* window and buffer
						(if (get-buffer "*TeX Help*")
								(progn
									(if (get-buffer-window (get-buffer "*TeX Help*"))
											(delete-windows-on "*TeX Help*"))
									(kill-buffer "*TeX Help*")))))))

		(general-define-key
		 :keymaps '(LaTeX-mode-map)
		 "C-x C-s" 'LaTeX-save-and-compile)

		(setq TeX-auto-save t)
		(setq TeX-parse-self t)
		(setq-default TeX-master nil)
		(add-hook 'LaTeX-mode-hook 'visual-line-mode)
		(add-hook 'LaTeX-mode-hook 'flyspell-mode)
		(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
		(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
		(setq reftex-plug-into-AUCTeX t)))

(use-package company-auctex
	:after auctex
	:ensure t)


(provide 'init-tex)
