; vim:set filetype=scheme foldmethod=marker:
; 関数 cxxr
(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define caar (lambda (x) (car (car x))))
(define cddr (lambda (x) (cdr (cdr x))))

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

; マッピング
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

; フィルター
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

(define _+ +)
(define +
  (lambda args
	(reduce _+ (car args) (cdr args))))
(define _- -)
(define -
  (lambda args
	(reduce _- (car args) (cdr args))))
(define _* *)
(define *
  (lambda args
	(reduce _* (car args) (cdr args))))
(define __/ /)
(define /
  (lambda args
	(reduce _/ (car args) (cdr args))))

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

(define (require name)
  (if (not (find (lambda (x) (equal? name x)) *provide-list*))
	(load (+ "lispy/" name ".scm"))
	#f))

