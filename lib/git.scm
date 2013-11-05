; vim:set filetype=scheme foldmethod=marker:
(provide "git")

(require-python "builtins")
(require-python "platform")
(require-python "command")
(require-python "logging")
(require-python "lispy")
(require-python "os.path")
(require "filer")

(in-package "git")
(define add
  (lambda files
	(let ((target (if (null? files)
					(map filer::file-name (filer::select-or-cursor-files))
					(files))))
	  (sh-call (filer::pwd) (append (list "git" "add") target)))))

(define (commit comment)
  (sh-call (filer::pwd) (list "git" "commit" "-m" comment)))

(in-package "global")
