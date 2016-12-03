; Change to your own location
(load "compiler.scm")
(load "tagparser.so")

(define my-parse-func tag-parse)
(define staff-parse-func parse)

(define try-catch
  (lambda (try-thunk catch-thunk)
    (guard (c (else (catch-thunk)))
     (try-thunk))))

(define testVSstaff
	(lambda (input)
		(let* ((my-res (try-catch (lambda () (my-parse-func input)) (lambda () (format "ERROR"))))
		      (staff-res (try-catch (lambda () (staff-parse-func input)) (lambda () (format "ERROR")))))
			(display input)
			(display ": ")			
			(cond ((equal? my-res staff-res)
				(display "\033[1;32mSuccess! ☺ \033[0m ") (newline) #t)
				(else (display "\033[1;31mFailed! ☹\033[0m ") 
					(display ", \nexpected: ")					
					(display staff-res)
					(display ", \nactual:   ")
					(display my-res)
					(newline)
					#f))
			)))
			
(define runTests
  (lambda (tests-name lst)
	(newline)
	(display tests-name)
	(display ":")
	(newline)
	(display "=============")
	(newline)
	(let ((results (map testVSstaff lst)))
	(newline)
	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32m") (display tests-name) (display " Tests: SUCCESS! ☺ \033[0m\n") (newline) #t)
		(else (display "\033[1;31m") (display tests-name) (display " Tests: FAILED! ☹ \033[0m\n") (newline) #f)))
))

(define runAllTests
  (lambda (lst)
    (let ((results (map (lambda (test) (runTests (car test) (cdr test))) lst)))
      	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32m !!!!!  ☺  ALL TESTS SUCCEEDED  ☺  !!!!\033[0m\n"))
		(else (display "\033[1;31m #####  ☹  SOME TESTS FAILED  ☹  #####\033[0m\n")))
		(newline))
))

(define ifTests
  (list
    `(if a b c)
    `(if a b)
    `(if a 4)
    `(if #t 'abc)
    `(if (if a b c) 'c '(x y z))
    `(if (if a b c) 'c (if "abc" 'x 123))    
))

(define condTests
  (list
     ;without sequences
     `(cond)
     `(cond (x 1))
     `(cond (x 1) (c 12))
     `(cond (x 1) (b 2) (c 3))
     
     ; with sequences
     `(cond (x 1 2))
     `(cond (x 1 2 abc) (c 12))
     `(cond (x 1 2 abc1) (b #f) (c (or 1 2) #t))
     `(cond (x 1 2 abc1) (b #f) (c (or 1 2) #t) (else #f))
     `(cond (x 1 2 abc1) (b #f) (c (or 1 2) #t) (else 112))
))

(define orTests
  (list
    `(or)
    `(or 1)
    `(or 1 2)
    `(or 1 2 3)
    `(or (or "abc" '123) (if a b c))
    `(or 'shaham)
    `(or "naimark")
    `(or "naimark" "bob")
    `(or 'naimark 'bob)
    `(or 'naimark "bob")
    `(or 'naimark 12)
        
))

(define andTests
  (list
    `(and)
    `(and 1)
    `(and 1 2)
    `(and 1 2 3 4)
    `(and 1 2 3 abc)
    `(and 1 2 3 13 4 5 4 7 3)
    `(and (or "abc" '123) (if a b c))
    `(and 'shaham "naimark")
    `(and "naimark")
))


(define variableTests
  (list
    'abc
    '123x
    'AbC1gfj5j959b949jm5b9gk5kg
    'let2*
    'set-bang!
))

(define letTests
  (list
    '(let () body)
    '(let ((x 1)) body)
    '(let ((x 1) (y 21)) body)
    '(let ((x 1) (y 21) (abcde fghijKlmnOP123)) body (if (> x 5) 4 3) #t)
    
    '(let* () body)
    '(let* ((x 1) (y 21)) body)
    '(let* ((x 1) (x abv)) body)
    '(let* ((x 1) (y 21) (abcde fghijKlmnOP123)) body (if (> x 5) 4 3) #t)
    
    '(letrec () body)
    '(letrec ((x 1)) b)
    '(letrec ((x 1) (y 2)) b1 b2)
    '(letrec ((x 1) (y 2) (a 5)) b1 b2 (or 1 2 3) (if 1 2 3) #t)
))

(define constantTests
  (list
    ''()   
    ''#(2)
    ''#(1 (2 3 4) 2 3)
    #f
    #\a  
    34
    "abc"
    '(quote a)
    '(quote (a b c))
    '(quote (quote a b c))
))

(define lambdaTests
  (list
    ; lambda-simple
    '(lambda () body)
    '(lambda (x) a)
    '(lambda (exp rest) (if a 1 "abc"))
    '(lambda (a b c d e f Symbol1) E1 E2 E3 E4 (f1 a))
    '(lambda (exp rest) (or a b c) (if a 1 "abc"))
    '(lambda (a b c d e arg154) (if a 1 "abc"))
    '(lambda () (or 1))
    '(lambda (Sym1 Symbol2 Symbol1234567890) (display "Akuna Matata"))
    
    ;lambda-opt
    '(lambda (x y . z) a)
    '(lambda (abc . def) abc)
    '(lambda (exp . rest) (or a b c) (if a 1 "abc"))
    
    ;lambda-Variables
    '(lambda args (if a b c))
    '(lambda args args)
    '(lambda args (or a b c) (if a 1 "abc"))
))

(define defineTests
  (list
    ;regular-define
    '(define x 5)
    '(define x (lambda (x) x))
    
    ;mit-style-define
    '(define (id x) x)
    '(define (foo x y z) (if x y z))
    '(define (foo x y . z) (if x y z))
    '(define (list . args) args)
))

(define applicationTests
  (list
    '(a)
    '(a b c)
    '((a b) (a c) (a d))
))

(define quasiquoteTests
  (list
    `(quasiquote (a b c))
    `(quasiquote (a ,b c))
    `(quasiquote (a ,b ,@c))    
    `(quasiquote (,@a ,b c))
    `(quasiquote (,@a ,@b ,@c))    
    `(quasiquote (,a ,b ,c))    
))

(define beginTests
  (list
    '(begin)
    '(begin 1)
    '(begin (or 1 2 3))
    '(begin (or 1 2) (if 1 2 3))    
))

(define setTests
  (list
    '(set! x 3)
    '(set! v (f x))
))

(define parserTests
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

(define bdTests
  (list
    'hello
    '(letrec () body)
))

(runAllTests
  (list
      (cons "BD tests" bdTests)     
      (cons "Lambda" lambdaTests)     
      (cons "Or" orTests)   
      (cons "And" andTests) 
      (cons "If" ifTests)    
      (cons "Cond" condTests)      
      (cons "Constants" constantTests)    
      (cons "Variables" variableTests)     
      (cons "Define" defineTests)  
      (cons "Let" letTests)   
      (cons "Begin" beginTests)  
      (cons "Set" setTests) 
      (cons "Application" applicationTests)    
      (cons "QuasiQuote" quasiquoteTests) 
      (cons "Parser" parserTests)
))