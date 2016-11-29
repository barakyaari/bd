(load "naive-qq.scm")
(load "pattern-matcher.scm")

(print-graph #f) ; display circular structures
(print-gensym #f) ; print gensym as g1234
(case-sensitive #f) ; ditto
(print-brackets #f) ; do not use brackets when pretty-printing

(revert-interaction-semantics) ; allow builtins to be redefined

;;; fix bug in optimizer
(#%$sputprop 'append '*flags* 122)
(#%$sputprop 'append! '*flags* 34)
(#%$sputprop 'list* '*flags* 1250)
(#%$sputprop 'cons* '*flags* 1250)

;;; And just for good luck :-)
(define with (lambda (s f) (apply f s)))

;;;;;;;;;;;;;;;;;; ASSIGNMENT 2 ;;;;;;;;;;;;;;;;;;

(define not!
	(lambda (expr)
		`(not ,expr)))

(define contains
	(lambda (lst item)
		(if (member item lst)
			#t
			#f)))

(define *reserved-words*
	'(and begin cond define do else if lambda
		let let* letrec or quasiquote unquote 
		unquote-splicing quote set! not))

(define not-reserved?
	(lambda (e)
		(not (contains *reserved-words* e))))
		
(define simple-const?
	(lambda (expr)
		(if (or (boolean? expr)
				(number? expr)
				(char? expr)
				(string? expr))
			#t
			#f)))

(define begin?
	(lambda (expr)
		(eq? expr 'begin)))
			
(define var?
	(lambda (expr)
		(if (and (symbol? expr) (not-reserved? expr))
			#t
			#f)))

(define make-void
	(lambda ()
		(if #f
			#f)))

(define get-required-param
	(lambda (p)
		(if (pair? p)
			(append (list (car p)) (get-required-param (cdr p)))
			'())))
			
(define get-rest-param
	(lambda (p)
		(if (pair? p)
			(get-rest-param (cdr p))
			p)))
			
; check that no item appears more than once in the list
(define valid-list?
	(lambda (lst)
		(if (null? lst)
			#t
			(and (not (contains (cdr lst) (car lst)))
				 (valid-list? (cdr lst))))))

				 
;;;;;;;;;;;;;;; parser ;;;;;;;;;;;;;;;
				 
(define parse
	(let ((run (compose-patterns
					(pattern-rule
						(? 'e null?)
						(lambda (e) '()))
					; const
					(pattern-rule
						(? 'e simple-const?)
						(lambda (e) `(const ,e)))
					(pattern-rule
						`(,(? 'e begin?))
						(lambda (e) `(const ,(make-void))))
					(pattern-rule
						`(quote ,(? 'e))
						(lambda (e) `(const ,e)))
					; var
					(pattern-rule
						(? 'v var?)
						(lambda (v)  `(var ,v)))
					; if without dif
					(pattern-rule
						`(if ,(? 'test) ,(? 'dit))
						(lambda (test dit)
							`(if3 ,(parse test) ,(parse dit) (const ,(make-void)))))
					; if with dif
					(pattern-rule
						`(if ,(? 'test) ,(? 'dit) ,(? 'dif))
						(lambda (test dit dif)
							`(if3 ,(parse test) ,(parse dit) ,(parse dif))))
					; sequence 
					(pattern-rule
						`(begin ,(? 'expr) . ,(? 'rest-exprs))
						(lambda (expr rest-exprs)
							`(seq (,(parse expr) ,@(map parse rest-exprs)))))
					; sequence 
					(pattern-rule
						`(set! ,(? 'expr) . ,(? 'rest-exprs))
						(lambda (expr rest-exprs)
							`(seq (,(parse expr) ,@(map parse rest-exprs)))))
					; lambda
					(pattern-rule
						`(lambda ,(? 'params) ,(? 'expr) . ,(? 'rest-exprs))
						(lambda (params expr rest-exprs)
							(parse-lambda params expr rest-exprs)))
					; define
					(pattern-rule
						`(define ,(? 'var) ,(? 'val))
						(lambda (var val)
							(if (symbol? var)
								`(define ,(parse var) ,(parse val)) ; regular define
								(parse (expand-MIT-define var val))))) ; MIT-style define
					; application
					(pattern-rule
						`(,(? 'foo not-reserved?) . ,(? 'args))
						(lambda (foo args)
							`(applic ,(parse foo) (,@(map parse args)))))
					; let
					(pattern-rule
						`(let ,(? 'varval) ,(? 'expr) . ,(? 'rest-exprs))
						(lambda (varval expr rest-exprs)
							(parse (expand-let varval expr rest-exprs))
							))
					; let*
					(pattern-rule
						`(let* () ,(? 'expr) . ,(? 'exprs list?))
						(lambda (expr exprs)
							(parse (expand-empty-let* (cons expr exprs)))))
					(pattern-rule
						`(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
						(lambda (var val rest exprs)
							(parse (expand-let* var val rest exprs))))
					;letrec
					(pattern-rule
						`(letrec ,(? 'varvals) . ,(? 'exprs))
						(lambda (varvals exprs)
							(parse (expand-letrec varvals (car exprs)))))
					; and
					(pattern-rule
						`(and . ,(? 'exprs))
						(lambda (exprs) (parse (expand-and exprs))))
					; or
					(pattern-rule
						`(or . ,(? 'exprs))
						(lambda (exprs) (parse-or exprs)))
					; cond
					(pattern-rule
						`(cond ,(? 'first-case) . ,(? 'rest-cases))
						(lambda (first-case rest-cases) (parse (expand-cond first-case rest-cases))))
					; not
					(pattern-rule
						`(not ,(? 'expr))
						(lambda (expr) `(not ,(parse expr))))
					; quasiqoute
					(pattern-rule
						`(quasiquote . ,(? 'e))
						(lambda (e)
							(if (= (length e) 1)
								(expand-qq (car e))
								(error 'quasiqoute "invalid syntax for quasiqoute!"))))
					; unqoute
					(pattern-rule
						`(unquote . ,(? 'e))
						(lambda (e) (parse (expand-qq e))))
					; unqoute-splicing
					(pattern-rule
						`(unquote-splicing ,(? 'e))
						(lambda (e) (parse (expand-qq e))))
					)))
		(lambda (e)
			(run e
				(lambda ()
					(error 'parse (format "I can't recognize this: ~s" e)))))))

					
;;;;;;;;;;;; Macro Expansions ;;;;;;;;;;;;

(define expand-MIT-define
	(lambda (var val)
		(let ((f (car var))
			  (params (cdr var)))
			`(define ,f (lambda ,params ,val)))))
					
(define expand-let
	(lambda (var-val expr rest-exprs)
		(let ((vars (map car var-val))
			  (vals (map cadr var-val))
			  (exprs (append (list expr) rest-exprs)))
			(if (valid-list? vars)
				`((lambda ,vars ,@exprs) ,@vals)
				(error 'let "The variables must all be different.")))))

(define expand-let*
	(lambda (var val rest exprs)
		(if (null? rest)
			`(let ((,var ,val)) . ,exprs)
			`(let ((,var ,val)) (let* ,rest . ,exprs)))))
				
(define expand-empty-let*
	(lambda (exprs)
		`(begin ,@exprs)))
				
(define expand-letrec
	(lambda (ribs . exprs)
		(let* ((fs (map car ribs))
			   (lambda-exprs (map cdr ribs))
			   (newfs `(,@fs))
			   (body-f `(lambda ,newfs ,@exprs))
			   (hofs (map (lambda (lambda-expr) `(lambda ,newfs ,@lambda-expr)) lambda-exprs)))
			(if (valid-list? fs)
				`(Ym ,body-f ,@hofs)
				(error 'letrec "The variables must all be different.")))))
			
(define expand-and
	(lambda (exprs)
		(cond ((null? exprs) '#t)
			  ((eq? (length exprs) 1) (car exprs))
			  (else `(if ,(car exprs) (and ,@(cdr exprs)) #f)))))

(define expand-cond
	(lambda (first rest)
		(let ((v1 (car first))
			  (e1 (cdr first)))
			(if (> (length e1) 1)
				(set! e1 (append (list 'begin) e1))
				(set! e1 (car e1)))
			(if (null? rest)
				(if (equal? v1 'else)
					e1
					`(if ,v1 ,e1))
				`(if ,v1 ,e1 ,(expand-cond (car rest) (cdr rest)))))))

(define parse-lambda
	(lambda (params expr rest-exprs)
		(let ((new-exprs '()))
			(if (null? rest-exprs)
				(set! new-exprs expr)
				(set! new-exprs (append (list 'begin) (list expr) rest-exprs)))
			(cond ((list? params)
					`(lambda-simple ,params
						,(parse new-exprs)))
				  ((pair? params)
					`(lambda-opt ,(get-required-param params) ,(get-rest-param params)
						,(parse new-exprs)))
				  (else
					`(lambda-variadic ,params
						,(parse new-exprs)))))))
				
(define parse-or
	(lambda (exprs)
		(cond ((null? exprs) (parse #f))
			  ((equal? (length exprs) 1) (parse (car exprs)))
			  (else `(or ,(map parse exprs))))))
			  

;;;;;;;;;;;;;;;;;; ASSIGNMENT 3 ;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; Lexical Addresses ;;;;;;;;;;;;

(define pe->lex-pe
	(lambda (pe)
		(update-lexical-addrs pe '() '('()))))

(define update-lexical-addrs
	(lambda (pe params env)
		(cond ((not (list? pe)) pe)
			  ((null? pe) '())
			  ((equal? 'var (car pe)) (get-lexical-addr (cadr pe) params env))
			  ((equal? 'lambda-simple (car pe))
					(let ((new-params (cadr pe))
						  (body (caddr pe)))
						`(lambda-simple ,new-params ,(update-lexical-addrs body new-params (extend-env new-params env)))))
			  ((equal? 'lambda-opt (car pe))
					(let ((new-params (append (cadr pe) (list (caddr pe))))
						  (body (cadddr pe)))
						`(lambda-opt ,(cadr pe) ,(caddr pe) ,(update-lexical-addrs body new-params (extend-env new-params env)))))
			  ((equal? 'lambda-variadic (car pe))
					(let ((new-params (list (cadr pe)))
						  (body (caddr pe)))
						`(lambda-variadic ,(cadr pe) ,(update-lexical-addrs body new-params (extend-env new-params env)))))
			  (else (map update-lexical-addrs pe (make-list (length pe) params) (make-list (length pe) env))))))

(define extend-env
	(lambda (x env)
		(append (list x) env)))
		
(define get-lexical-addr
	(lambda (var params env)
		(if (contains params var) 
			`(pvar ,var ,(find-location params var 0))
			(lookup var (cdr env) 0))))
			  
(define find-location
	(lambda (lst item depth)
		(cond ((null? lst) -1)
			  ((equal? (car lst) item) depth)
			  (else (find-location (cdr lst) item (+ depth 1))))))
			  
(define lookup
	(lambda (var env depth)
		(cond ((null? env) `(fvar ,var))
			  ((contains (car env) var) `(bvar ,var ,depth ,(find-location (car env) var 0)))
			  (else (lookup var (cdr env) (+ depth 1))))))

;;;;;;;;;;;; Annotated Tale Position ;;;;;;;;;;;;

(define annotate-tc
	(lambda (pe)
		(atp pe #f)))
		
(define atp
	(lambda (pe tp?)
		(let ((tag (car pe)))
			(cond ((equal? 'const tag) pe)
				  ((or (equal? 'var tag) (equal? 'fvar tag) (equal? 'bvar tag) (equal? 'pvar tag)) pe)
				  ((equal? 'if3 tag) 
						(with (cdr pe)
							(lambda (test dit dif)
								`(if3 ,(atp test #f)
									  ,(atp dit tp?)
									  ,(atp dif tp?)))))
				  ((equal? 'or tag)
						(with (cdr pe)
							(lambda (items)
								(let ((last-item (car (reverse items)))
									  (first-items (reverse (cdr (reverse items)))))
									`(or ,(map atp first-items (make-list (length first-items) #f)) ,(atp last-item tp?))))))
				  ((equal? 'seq tag)
						(with (cdr pe)
							(lambda (items)
								(let ((last-item (car (reverse items)))
									  (first-items (reverse (cdr (reverse items)))))
									`(seq ,(map atp first-items (make-list (length first-items) #f)) ,(atp last-item tp?))))))
				  ((equal? 'define tag)
						(with (cdr pe)
							(lambda (var value)
								`(define ,var ,(atp value #f)))))
				  ((equal? 'lambda-simple tag)
						(with (cdr pe)
							(lambda (params body)
								`(lambda-simple ,params ,(atp body #t)))))
				  ((equal? 'lambda-opt tag)
						(with (cdr pe)
							(lambda (params rest body)
								`(lambda-opt ,params ,rest ,(atp body #t)))))
				  ((equal? 'lambda-variadic tag)
						(with (cdr pe)
							(lambda (params body)
								`(lambda-variadic ,params ,(atp body #t)))))								
				  ((equal? 'applic tag)
						(with (cdr pe)
							(lambda (proc args)
								(if tp?
									`(tc-applic ,(atp proc #f) ,(map atp args (make-list (length args) #f)))
									`(applic ,(atp proc #f) ,(map atp args (make-list (length args) #f)))))))))))
							
(define test1
	(lambda (e)
		(annotate-tc (pe->lex-pe (parse e)))))
				
(define test2
	(lambda (e)
		(pe->lex-pe (annotate-tc (parse e)))))
