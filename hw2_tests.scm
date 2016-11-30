(load "compiler.scm")

(define try-catch
  (lambda (try-thunk catch-thunk)
    (guard (c (else (catch-thunk)))
     (try-thunk))))
	 
(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (vector? e)
	    (symbol? e))
	`',e
	e)))



(define test-parser-guarded
  (lambda ()
    (apply append
     (map
         (lambda (input expected-output)
     (let ((result (try-catch (lambda () (tag-parse input))
                              (lambda () (format "excpetion while processing ~s" input)))))
       (if (equal? result expected-output)
           '()
           `((error: (tag-parse ,(quotify input)) was supposed to generate ,expected-output but generated ,result instead!)))))
       *parser-tests* *parser-results*))))

(define *parser-tests*
  '(4 
	-3 
	#t 
	#f 
	#\a 
	#\A 
	#\space 
	#\tab
	'3 
	3 
	'"abc" 
	"abc" 
	'#\a 
	#\a 
	''a 
	''''''a
	'abc 
	abc 
	'#(a b c) 
	'#() 
	'#(() 
	(#()))
	(a b c)
	(if (zero? n) 1 (* n (fact (- n 1))))
	(cond ((foo? x) foo-x)
		((goo? x) goo-x)
		((boo? x) boo-x)
		(else poo-x))
	(begin e1)
	(begin e1 e2 e3)
	(lambda a b c)
	(lambda a b)
	(lambda (a b . c) (list a b c))
	(lambda (a b c) (list a b c))
	(let ((a 1) (b 2) (c 3))
	  (+ a b c))
	(let* ((a 1)
		   (a (+ a 1))
		   (a (+ a a))
		   (a (* a a)))
	  (set-car! x a)
	  a)
	(define a 3)
	(define a (lambda (x) x))
	(define (fact n) (if (zero? n) 1 (* n (fact (- n 1)))))
	(define (foo a b . c) (list a b c))
	(define (foo . a) (list a))
	(define (foo a b c) (list a b c))
))

(define *parser-results*
  '((const 4) 
    (const -3) 
	(const #t) 
	(const #f) 
	(const #\a)
	(const #\A) 
	(const #\space) 
	(const #\tab) 
	(const 3)
	(const 3) 
	(const "abc") 
	(const "abc") 
	(const #\a)
	(const #\a) 
	(const 'a) 
	(const '''''a) 
	(const abc) 
	(var abc)
	(const #(a b c)) 
	(const #()) 
	(const #(() (#())))
	(applic (var a) ((var b) (var c)))
	(if3 (applic (var zero?) ((var n)))
		  (const 1)
		  (applic
			(var *)
			((var n)
			  (applic
				(var fact)
				((applic (var -) ((var n) (const 1))))))))
	 (if3 (applic (var foo?) ((var x)))
		  (var foo-x)
		  (if3 (applic (var goo?) ((var x)))
			   (var goo-x)
			   (if3 (applic (var boo?) ((var x)))
					(var boo-x)
					(var poo-x))))
	 (var e1) 
	 (seq ((var e1) (var e2) (var e3)))
	 (lambda-variadic a (seq ((var b) (var c))))
	 (lambda-variadic a (var b))
	 (lambda-opt
	   (a b)
	   c
	   (applic (var list) ((var a) (var b) (var c))))
	 (lambda-simple
	   (a b c)
	   (applic (var list) ((var a) (var b) (var c))))
	 (applic
	   (lambda-simple
		 (a b c)
		 (applic (var +) ((var a) (var b) (var c))))
	   ((const 1) (const 2) (const 3)))
	 (applic
	   (lambda-simple
		 (a)
		 (applic
		   (lambda-simple
			 (a)
			 (applic
			   (lambda-simple
				 (a)
				 (applic
				   (lambda-simple
					 (a)
					 (seq ((applic (var set-car!) ((var x) (var a))) (var a))))
				   ((applic (var *) ((var a) (var a))))))
			   ((applic (var +) ((var a) (var a))))))
		   ((applic (var +) ((var a) (const 1))))))
	   ((const 1)))
	 (define (var a) (const 3))
	 (define (var a) (lambda-simple (x) (var x)))
	 (define (var fact)
		  (lambda-simple
			(n)
			(if3 (applic (var zero?) ((var n)))
				 (const 1)
				 (applic
				   (var *)
				   ((var n)
					 (applic
					   (var fact)
					   ((applic (var -) ((var n) (const 1))))))))))
	 (define (var foo)
		  (lambda-opt
			(a b)
			c
			(applic (var list) ((var a) (var b) (var c)))))
	 (define (var foo) (lambda-variadic a (applic (var list) ((var a)))))
	 (define (var foo)
		  (lambda-simple
			(a b c)
			(applic (var list) ((var a) (var b) (var c)))))
 ))

(test-parser-guarded)
