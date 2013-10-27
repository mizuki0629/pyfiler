; vim:set filetype=scheme foldmethod=marker:
; cxxr {{{
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (caar x)))
(define (caadr x) (car (cadr x)))
(define (cadar x) (car (cdar x)))
(define (caddr x) (car (cddr x)))
(define (cdaar x) (cdr (caar x)))
(define (cdadr x) (cdr (cadr x)))
(define (cddar x) (cdr (cdar x)))
(define (cdddr x) (cdr (cddr x)))

(define (caaaar x) (car (caaar x)))
(define (caaadr x) (car (caadr x)))
(define (caadar x) (car (cadar x)))
(define (caaddr x) (car (caddr x)))
(define (cadaar x) (car (cdaar x)))
(define (cadadr x) (car (cdadr x)))
(define (caddar x) (car (cddar x)))
(define (cadddr x) (car (cdddr x)))
(define (cdaaar x) (cdr (caaar x)))
(define (cdaadr x) (cdr (caadr x)))
(define (cdadar x) (cdr (cadar x)))
(define (cdaddr x) (cdr (caddr x)))
(define (cddaar x) (cdr (cdaar x)))
(define (cddadr x) (cdr (cdadr x)))
(define (cdddar x) (cdr (cddar x)))
(define (cddddr x) (cdr (cdddr x)))
; }}}

(define-macro let*
  (lambda args
	(let ((ag (car args))
		 (body (cdr args)))
	  (if (null? (cdr ag))
		`(let (,(car ag)) ,@body)
		`(let (,(car ag)) (let* ,(cdr ag) ,@body))))))

(define-macro letrec
			  (lambda a
				(let* ((args (car a))
					   (body (cdr a))
					   (vars (map car args))
					   (vals (map cadr args)))
				  `(let ,(map (lambda (x) `(,x '*undef*)) vars)
					 ,@(map-2 (lambda (x y) `(set! ,x ,y)) vars vals)
					 ,@body))))

(define-macro cond
  (lambda args
    (if (null? args)
        '*undef*
      (if (eq? (caar args) 'else)
          `(begin ,@(cdar args))
        (if (null? (cdar args))
            `(let ((+value+ ,(caar args)))
              (if +value+ +value+ (cond ,@(cdr args))))
          `(if ,(caar args)
               (begin ,@(cdar args))
            (cond ,@(cdr args))))))))

(define (assq x y)
  (cond ((null? y) nil)
		((eq? x (caar y)) (car y))
		(#t (assoc x (cdr y))) ))

(define (assoc x y)
  (cond ((null? y) nil)
		((equal? x (caar y)) (car y))
		(#t (assoc x (cdr y))) ))

(define (filter predicate ls)
  (cond ((null? ls) nil)
		((predicate (car ls))
		 (cons (car ls) (filter predicate (cdr ls))))
		(else
		  (filter predicate (cdr ls)))))

(define map
  (lambda (fn ls)
    (if (null? ls)
        '()
      (cons (fn (car ls)) (map fn (cdr ls))))))

(define map-2
  (lambda (fn xs ys)
    (if (null? xs)
        '()
      (cons (fn (car xs) (car ys)) (map-2 fn (cdr xs) (cdr ys))))))

(define filter
  (lambda (fn ls)
    (if (null? ls)
        '()
      (if (fn (car ls))
          (cons (car ls) (filter fn (cdr ls)))
        (filter fn (cdr ls))))))

(define for-each
  (lambda (fn ls)
    (if (null? ls) '()
	  (begin
		(fn (car ls))
		(for-each fn (cdr ls))))))

(define (reduce f g ls)
  (if (null? ls)
	g
	(reduce f (f g (car ls)) (cdr ls))))

(require-python "operator")
(define (+ init . ls)
	(reduce operator::add init ls))
(define (- init . ls)
	(reduce operator::sub init ls))
(define (* init . ls)
	(reduce operator::mul init ls))
(define (/ init . ls)
	(reduce operator::truediv init ls))

(define (find predicate lis)
  (define (iter ls)
	(cond ((null? ls) #f)
		  ((predicate (car ls))
		   (car ls))
		  (else
			(iter (cdr ls)))))
  (iter lis))

(define *provide-list* nil)
(define (provide name)
	(set! *provide-list* (cons name *provide-list*)))

; TODO load-pathを実装
(define *load-path*
  '(
	"lib"
	""
	))
(define (require name)
  (if (not (find (lambda (x) (equal? name x)) *provide-list*))
	(load (+ "lib/" name ".scm"))
	#f))

