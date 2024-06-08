(defcustom *switch-lisp-window-side* 'right "Side on which to open the new window on.  Corresponds to the SPLIT-WINDOW functions."
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
(defcustom *switch-lisp-common-lisp-repl-command* 'slime "Command to invoke if a Common Lisp REPL isn't found for a Common Lisp file."
	:group 'programming
	:type '(function))
(defcustom *switch-lisp-scheme-repl-command* 'geiser "Command to invoke if a Scheme REPL isn't found for a Scheme file."
	:group 'programming
	:type '(function))
(defcustom *switch-lisp-clojure-repl-command* 'cider-jack-in "Command to invoke if a Clojure REPL isn't found for a Clojure file."
	:group 'programming
	:type '(function))

;; TODO: either project or server-based detection of which files belong to which REPL instance.
;; TODO: only delete REPL windows on default case
(defun switch-lisp (buffer)
	"Switch between Lisp buffers and REPLs depending on BUFFER."
	(interactive "bBuffer: ")
	(let* ((buf (get-buffer buffer))
				 (file (buffer-file-name buf))
				 (extension (if file (file-name-extension file) nil)))
		(cond
		 ((string= ".lisp" extension) (progn (require 'sly) (switch-lisp--open-side-buffer-if-not-exists "*slime*" *switch-lisp-common-lisp-repl-command* buffer)))
		 ((string= ".scm" extension) (progn (require 'geiser) (switch-lisp--open-side-buffer-if-not-exists "*geiser*" *switch-lisp-scheme-repl-command* buffer)))
		 ((string= ".clj" extension) (progn (require 'cider) (switch-lisp--open-side-buffer-if-not-exists "*cider-repl cider-nrepl*" *switch-lisp-clojure-repl-command*)))
		 (t (progn (switch-to-buffer buf) (if *switch-lisp-delete-windows-on-non-lisp* (delete-other-windows)))))))

(provide 'switch-lisp)

