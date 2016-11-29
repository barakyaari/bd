(load "naive-qq.scm")
(load "pattern-matcher.scm")

;;;;;;;;;;;;;;;;;; Auxiliaries ;;;;;;;;;;;;;;;;;;

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

				 
;;;;;;;;;;;;;;;;;; parser ;;;;;;;;;;;;;;;;;;
				 
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
					
					; define
					(pattern-rule
						`(define ,(? 'var) ,(? 'val))
						(lambda (var val)
							(if (symbol? var)
								; regular define
								`(define ,(parse var) ,(parse val))
								; MIT-style define
								(parse (expand-MIT-define var val)))))
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
						(lambda (exprs) `(or ,(map parse exprs))))
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

					
;;;;;;;;;;;;;;; Macro Expansions ;;;;;;;;;;;;;;;

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
				(set! e1 (append (list 'begin) (cdr first)))
				(set! e1 (car e1)))
			(if (null? rest)
				(if (eq? v1 'else)
					e1
					`(if ,v1 ,e1))
				`(if ,v1 ,e1 ,(expand-cond (car rest) (cdr rest)))))))
				
 (parse '(if a b))
