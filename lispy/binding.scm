; vim:set filetype=scheme foldmethod=marker:

(define (ref obj attr)
  (py-attr attr obj))

(define-macro $mcall
	(lambda args
	  `(py-method-call (quote ,(car args)) ,@(cdr args))))

