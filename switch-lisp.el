(require 'cl-lib)

(defcustom *switch-lisp-window-side* 'right
	"Side on which to open the new window on.  Corresponds to the SPLIT-WINDOW functions."
	:group 'programming
	:type '(choice (const :tag "Right" right)
								 (const :tag "Below" below)
								 (const :tag "Sensibly" sensibly)
								 (const :tag "Vertically" vertically)
								 (const :tag "Horizontally" horizontally)))

;; TODO: test
(defun switch-lisp--open-side-buffer-if-not-exists (buffer-to-check new-command &optional buffer-to-use)
	"Switch to BUFFER-TO-USE, open a new window to the side specified by `*window-side*' and check if the buffer BUFFER-TO-CHECK is open.  If it is, switch to BUFFER-TO-CHECK, otherwise run NEW-COMMAND."
	(or buffer-to-use (setq buffer-to-use (current-buffer)))
	(switch-to-buffer buffer-to-use)
	(with-selected-window (split-window-right)
		(if (get-buffer buffer-to-check)
				(switch-to-buffer buffer-to-check)
			(funcall new-command))))

(defcustom *switch-lisp-delete-windows-on-non-lisp* t "If t, delete REPL windows when switching to any other language or to elisp."
	:group 'programming
	:type '(boolean))

;; Use hash table for all languages (key: language, value: (extension command feature buffer), loop through all entries in `switch-lisp'
;; TODO: change to struct

(cl-defstruct switch-lisp--language-data
	"Structure used in `*switch-lisp-languages*'."
	(name nil :type string)
	(extension nil :type string)
	(command nil :type symbol)
	(feature nil :type symbol)
	(buffer nil :type string))

(defcustom *switch-lisp-languages* (list (make-switch-lisp--language-data :name "Common Lisp" :extension ".lisp" :command #'slime :feature 'slime :buffer "*slime*")
																				 (make-switch-lisp--language-data :name "Scheme" :extension ".scm" :command #'geiser :feature 'geiser :buffer "*geiser*")
																				 (make-switch-lisp--language-data :name "Clojure" :extension ".clj" :command #'cider-jack-in :feature 'cider :buffer "*cider-repl cider-nrepl*"))
	"A list of languages used by `switch-lisp'.  Each element is of type `switch-lisp--language-data'.  If you wish to add your own languages, add your language to this variable and, if you need more advanced functionality (like supporting multiple REPLs), define advice around `switch-lisp'."
	:group 'programming
	:type '(list))

;; TODO: either project or server-based detection of which files belong to which REPL instance.
;; TODO: only delete REPL windows on default case
;; TODO: test
(defun switch-lisp (buffer)
	"Switch between Lisp buffers and REPLs depending on BUFFER."
	(interactive "bBuffer: ")
	(let* ((buf (get-buffer buffer))
				 (file (buffer-file-name buf))
				 (extension (if file (file-name-extension file) nil)))
		(dolist (current-language *switch-lisp-languages*)
			(when (string= (switch-lisp--language-data-extension current-language) extension)
				(progn (require (switch-lisp--language-data-feature current-language))
							 (switch-lisp--open-side-buffer-if-not-exists
								(switch-lisp--language-data-buffer current-language)
								(switch-lisp--language-data-command current-language) buffer))))))

(provide 'switch-lisp)

