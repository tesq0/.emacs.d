(require 'gtags)
(define-prefix-command 'gtags-map)
(general-define-key
 :keymaps 'gtags-map
 "b"  'gtags-display-browser
 "P"  'gtags-find-file
 "f"  'gtags-parse-file
 "g"  'gtags-find-with-grep
 "i"  'gtags-find-with-idutils
 "s"  'gtags-find-symbol
 "r"  'gtags-find-rtag
 "t"  'gtags-find-tag
 "h"  'gtags-find-tag-from-here
 "e"  'gtags-find-tag-by-event
 "o"  'gtags-find-tag-other-window
 "d"  'gtags-visit-rootdir)
(mikus-leader
	:states '(normal motion)
	"t" 'gtags-map)

(provide 'init-tags)
