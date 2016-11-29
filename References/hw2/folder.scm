(load "pattern-matcher.scm")

(define make-void
	(lambda ()
		(if #f
			#f)))

(define contains?
	(lambda (lst item)
		(if (member item lst)
			#t
			#f)))
		
(define number-application?
	(lambda (expr)
		(and (list? expr) (contains? (list '+ '* 'add1 'sub1) (car expr)))))
		
(define not-list?
	(lambda (e)
		(not (list? e))))
		
(define fold
	(let ((run (compose-patterns
					; null
					(pattern-rule
						(? 'e null?)
						(lambda (e) '()))
					; number or variable
					(pattern-rule
						(? 'e not-list?)
						(lambda (e) e))
					; quote
					(pattern-rule
						`(quote ,(? 'e))
						(lambda (e) `',e))
					; +
					(pattern-rule
						`(+ . ,(? 'args))
						(lambda (args) (fold-plus args)))
					; *
					(pattern-rule
						`(* . ,(? 'args))
						(lambda (args) (fold-mult args)))
					; zero?
					(pattern-rule
						`(zero? . ,(? 'arg))
						(lambda (arg) (fold-zero arg)))
					; add1
					(pattern-rule
						`(add1 . ,(? 'arg))
						(lambda (arg) (fold-add1 arg)))
					; sub1
					(pattern-rule
						`(sub1 . ,(? 'arg))
						(lambda (arg) (fold-sub1 arg)))
					; list
					(pattern-rule
						`(list . ,(? 'args))
						(lambda (args) (fold-list args)))
					; cons
					(pattern-rule
						`(cons . ,(? 'args))
						(lambda (args) (fold-cons args)))
					; null?
					(pattern-rule
						`(null? . ,(? 'args))
						(lambda (args) (fold-null args)))
					; number?
					(pattern-rule
						`(number? . ,(? 'args))
						(lambda (args) (fold-number args)))
					; string?
					(pattern-rule
						`(string? . ,(? 'args))
						(lambda (args) (fold-string args)))
					; string-append
					(pattern-rule
						`(string-append . ,(? 'args))
						(lambda (args) (fold-string-append args)))
					; append
					(pattern-rule
						`(append . ,(? 'args))
						(lambda (args) (fold-append args)))
						
					; if without dif
					(pattern-rule
						`(if ,(? 'test) ,(? 'dit))
						(lambda (test dit) (fold-if (fold test) (fold dit) (make-void))))
					; if with dif
					(pattern-rule
						`(if ,(? 'test) ,(? 'dit) ,(? 'dif))
						(lambda (test dit dif) (fold-if (fold test) (fold dit) (fold dif))))
					; car / cdr
					(pattern-rule
					   `(car (car (car (car ,(? 'expr)))))
					   (lambda (expr) (fold-car-cdr caaaar 'caaaar expr)))
					 (pattern-rule
					   `(car (car (car (cdr ,(? 'expr)))))
					   (lambda (expr) (fold-car-cdr caaadr 'caaadr expr)))
					 (pattern-rule
					   `(car (car (cdr (car ,(? 'expr)))))
					   (lambda (expr) (fold-car-cdr caadar 'caadar expr)))
					 (pattern-rule
					   `(car (car (cdr (cdr ,(? 'expr)))))
					   (lambda (expr) (fold-car-cdr caaddr 'caaddr expr)))
					 (pattern-rule
					   `(car (cdr (car (car ,(? 'expr)))))
					   (lambda (expr) (fold-car-cdr cadaar 'cadaar expr)))
					 (pattern-rule
					   `(car (cdr (car (cdr ,(? 'expr)))))
					   (lambda (expr) (fold-car-cdr cadadr 'cadadr expr)))
					 (pattern-rule
					   `(car (cdr (cdr (car ,(? 'expr)))))
					   (lambda (expr) (fold-car-cdr caddar 'caddar expr)))
					 (pattern-rule
					   `(car (cdr (cdr (cdr ,(? 'expr)))))
					   (lambda (expr) (fold-car-cdr cadddr 'cadddr expr)))
					 (pattern-rule
					   `(cdr (car (car (car ,(? 'expr)))))
					   (lambda (expr) (fold-car-cdr cdaaar 'cdaaar expr)))
					 (pattern-rule
					   `(cdr (car (car (cdr ,(? 'expr)))))
					   (lambda (expr) (fold-car-cdr cdaadr 'cdaadr expr)))
					 (pattern-rule
					   `(cdr (car (cdr (car ,(? 'expr)))))
					   (lambda (expr) (fold-car-cdr cdadar 'cdadar expr)))
					 (pattern-rule
					   `(cdr (car (cdr (cdr ,(? 'expr)))))
					   (lambda (expr) (fold-car-cdr cdaddr 'cdaddr expr)))
					 (pattern-rule
					   `(cdr (cdr (car (car ,(? 'expr)))))
					   (lambda (expr) (fold-car-cdr cddaar 'cddaar expr)))
					 (pattern-rule
					   `(cdr (cdr (car (cdr ,(? 'expr)))))
					   (lambda (expr) (fold-car-cdr cddadr 'cddadr expr)))
					 (pattern-rule
					   `(cdr (cdr (cdr (car ,(? 'expr)))))
					   (lambda (expr) (fold-car-cdr cdddar 'cdddar expr)))
					 (pattern-rule
					   `(cdr (cdr (cdr (cdr ,(? 'expr)))))
					   (lambda (expr) (fold-car-cdr cddddr 'cddddr expr)))
					 (pattern-rule
					   `(car (car (car ,(? 'expr))))
					   (lambda (expr) (fold-car-cdr caaar 'caaar expr)))
					 (pattern-rule
					   `(car (car (cdr ,(? 'expr))))
					   (lambda (expr) (fold-car-cdr caadr 'caadr expr)))
					 (pattern-rule
					   `(car (cdr (car ,(? 'expr))))
					   (lambda (expr) (fold-car-cdr cadar 'cadar expr)))
					 (pattern-rule
					   `(car (cdr (cdr ,(? 'expr))))
					   (lambda (expr) (fold-car-cdr caddr 'caddr expr)))
					 (pattern-rule
					   `(cdr (car (car ,(? 'expr))))
					   (lambda (expr) (fold-car-cdr cdaar 'cdaar expr)))
					 (pattern-rule
					   `(cdr (car (cdr ,(? 'expr))))
					   (lambda (expr) (fold-car-cdr cdadr 'cdadr expr)))
					 (pattern-rule
					   `(cdr (cdr (car ,(? 'expr))))
					   (lambda (expr) (fold-car-cdr cddar 'cddar expr)))
					 (pattern-rule
					   `(cdr (cdr (cdr ,(? 'expr))))
					   (lambda (expr) (fold-car-cdr cdddr 'cdddr expr)))
					 (pattern-rule
					   `(car (car ,(? 'expr)))
					   (lambda (expr) (fold-car-cdr caar 'caar expr)))
					 (pattern-rule
					   `(car (cdr ,(? 'expr)))
					   (lambda (expr) (fold-car-cdr cadr 'cadr expr)))
					 (pattern-rule
					   `(cdr (car ,(? 'expr)))
					   (lambda (expr) (fold-car-cdr cdar 'cdar expr)))
					 (pattern-rule
					   `(cdr (cdr ,(? 'expr)))
					   (lambda (expr) (fold-car-cdr cddr 'cddr expr)))
					 (pattern-rule
					   `(car ,(? 'expr))
					   (lambda (expr) (fold-car-cdr car 'car expr)))
					 (pattern-rule
					   `(cdr ,(? 'expr))
					   (lambda (expr) (fold-car-cdr cdr 'cdr expr)))
					)))
		(lambda (e)
			(run e
				(lambda ()
					(error 'fold (format "I can't recognize this: ~s" e)))))))

;;;;;;;;;;;;;;;;;;;; + ;;;;;;;;;;;;;;;;;;;;
				
(define fold-plus
	(lambda (args)
		(let* ((folded-args (apply append (map simplify-arg-plus (map fold args))))
			   (numbers (filter number? folded-args))
			   (symbols (filter symbol? folded-args))
			   (applics (filter number-application? folded-args))
			   (invalid-args (remp legal-arg? folded-args))
			   (new-args (append (calc-nums + numbers) (reduce symbols) (reduce applics))))
			(cond ((not (null? invalid-args)) (error '+ (format "invalid arguments: ~s" invalid-args)))
				  ((null? new-args) (apply + numbers))
				  ((eq? (length new-args) 1) (car new-args))
				  (else `(+ ,@new-args))))))

(define legal-arg?
	(lambda (arg)
		(or (number? arg) (symbol? arg) (number-application? arg))))

; (build-list 3 y) => (y y y)
; (build-list 2 (* x y)) => ((* x y) (* x y))
(define build-list
	(lambda (n e)
		(if (eq? n 1)
			(list e)
			(append (list e) (build-list (- n 1) e)))))

; (+ 1 x) => (1 x)
; (* 2 x) => (x x)
; (add1 x) => (1 x)
; (sub1 x) => (-1 x)
(define simplify-arg-plus
	(lambda (arg)
		(if (number-application? arg)
			(let ((op (car arg))
				  (applic-args (cdr arg)))
				(cond ((equal? op '+) applic-args)
					  ((equal? op '*)
							(if (null? (filter number? applic-args))
								(list arg)
								(if (eq? (length (remp number? applic-args)) 1)
									(build-list (car (filter number? applic-args)) (car (remp number? applic-args)))
									(build-list (car (filter number? applic-args)) `(* ,@(remp number? applic-args))))))
					  ((equal? op 'add1) (append (list 1) applic-args))
					  ((equal? op 'sub1) (append (list -1) applic-args))))
			(list arg))))

; (+ 1 2 3) => (6)
; (+ 3 -3) => '()
; (* 1 2 3) => (6)
; (* 1 1) => '()
(define calc-nums
	(lambda (func args)
		(let ((res (apply func (filter number? args))))
			(if (or (and (equal? func +) (eq? res 0))
					(and (equal? func *) (eq? res 1)))
				'()
				(list res)))))

; (x y x z z z) => ((* 2 x) y (* 3 z))
(define reduce
	(lambda (lst)
		(if (null? lst)
			'()
			(let*((v1 (car lst))
				  (v1-list (filter (lambda(v) (equal? v v1)) lst))
				  (v1-num (length v1-list))
				  (rest (remp (lambda(v) (equal? v v1)) lst)))
				  (if (> v1-num 1)
						(if (and (list? v1) (equal? (car v1) '*))
							(append `((* ,v1-num ,@(cdr v1))) (reduce rest))
							(append `((* ,v1-num ,v1)) (reduce rest)))
						(append (list v1) (reduce rest)))))))

;;;;;;;;;;;;;;;;;;;; * ;;;;;;;;;;;;;;;;;;;;

(define fold-mult
	(lambda (args)
		(let* ((folded-args (apply append (map simplify-arg-mult (map fold args))))
			   (numbers (filter number? folded-args))
			   (symbols (filter symbol? folded-args))
			   (applics (filter number-application? folded-args))
			   (invalid-args (remp legal-arg? folded-args))
			   (new-args (append (calc-nums * numbers) symbols applics)))
			(cond ((not (null? invalid-args)) (error '* (format "invalid arguments: ~s" invalid-args)))
				  ((null? new-args) (apply * numbers))
				  ((eq? (length new-args) 1) (car new-args))
				  (else `(* ,@new-args))))))

; (* 2 x) => (2 x)
(define simplify-arg-mult		  
	(lambda (arg)
			(if (and (number-application? arg) (equal? (car arg) '*))
				(cdr arg)
				(list arg))))
						
;;;;;;;;;;;;;;;;;;;; ZERO? ;;;;;;;;;;;;;;;;;;;;
					  
(define fold-zero
	(lambda (args)
		(if (eq? (length args) 1)
			(let ((folded-arg (fold (car args))))
				(cond ((symbol? folded-arg) `(zero? ,folded-arg))
					  ((and (list? folded-arg) (number-application? (car folded-arg))) `(zero? ,folded-arg))
					  (else (zero? folded-arg))))
			(error func-sym "incorrect number of arguments"))))

;;;;;;;;;;;;;;;;;;;; ADD1 ;;;;;;;;;;;;;;;;;;;;
		
(define fold-add1
	(lambda (arg)
		(if (> (length arg) 1)
			(error 'add1 "incorrect number of arguments")
			(let ((folded-arg (fold (car arg))))
				(if (or (number? folded-arg) (number-application? folded-arg))
					(fold `(+ 1 ,folded-arg))
					(if (symbol? folded-arg)
						`(add1 ,folded-arg)
						(error 'add1 (format "invalid argument: ~a" folded-arg))))))))

;;;;;;;;;;;;;;;;;;;; SUB1 ;;;;;;;;;;;;;;;;;;;;
						
(define fold-sub1
	(lambda (arg)
		(if (> (length arg) 1)
			(error 'add1 "incorrect number of arguments")
			(let ((folded-arg (fold (car arg))))
				(if (or (number? folded-arg) (number-application? folded-arg))
					(fold `(+ -1 ,folded-arg))
					(if (symbol? folded-arg)
						`(sub1 ,folded-arg)
						(error 'sub1 (format "invalid argument: ~a" folded-arg))))))))

;;;;;;;;;;;;;;;;;;;; LIST ;;;;;;;;;;;;;;;;;;;;
			
(define is-applic?
	(lambda (e)
		(and (list? e) (not (quote-list? e)) (not (null? e)))))
			
(define fold-list
	(lambda (args)
		(let* ((folded-args (map fold args))
				(applic-args (filter is-applic? folded-args)))
			(if (null? applic-args)
				`',(apply list folded-args)
				`(list ,@folded-args)))))
			
;;;;;;;;;;;;;;;;;;;; CONS ;;;;;;;;;;;;;;;;;;;;
			
(define quote?
	(lambda (e)
		(and (list? e) (eq? (car e) 'quote))))
		
(define quote-list?
	(lambda (e)
		(and (list? e) (eq? (car e) 'quote) (list? (cadr e)))))
			
(define get-quoted-e
	(lambda (e)
		(if (quote? e)
			(if (list? (cadr e))
				(cadr e)
				`',(cadr e))
			e)))
			
(define fold-cons
	(lambda (args)
		(if (eq? (length args) 2)
			(let ((a (fold (car args)))
				  (b (fold (cadr args))))
				(cond ((and (symbol? a) (quote-list? b) (null? (cadr b))) `(list ,a))
					  ((or (symbol? a) (symbol? b)) `(cons ,a ,b))
					  ((and (quote? a) (quote? b)) `'(,@(append (get-quoted-e a) (get-quoted-e b))))
					  (else `',(cons a b))))
			(error 'sub1 "incorrect number of arguments"))))
			
;;;;;;;;;;;;;;;;;;;; NULL? ;;;;;;;;;;;;;;;;;;;;
			
(define fold-null
	(lambda (args)
		(if (eq? (length args) 1)
			(let ((folded-arg (fold (car args))))
				(if (symbol? folded-arg) 
					(if (and (list? folded-arg) (eq? (car arg) 'quote))
						(null? (cadr arg))
						`(null? ,folded-arg))
					(null? (get-quoted-e folded-arg))))
		(error 'null? "incorrect number of arguments"))))
		
;;;;;;;;;;;;;;;;;;;; NUMBER? ;;;;;;;;;;;;;;;;;;;;
		
(define fold-number
	(lambda (args)
		(if (eq? (length args) 1)
			(let ((folded-arg (fold (car args))))
				(cond ((symbol? folded-arg) `(number? ,folded-arg))
					  ((and (list? folded-arg) (number-application? (car folded-arg))) #t)
					  (else (number? folded-arg))))
		(error 'number? "incorrect number of arguments"))))
		
;;;;;;;;;;;;;;;;;;;; STRING? ;;;;;;;;;;;;;;;;;;;;
		
(define fold-string
	(lambda (args)
		(if (eq? (length args) 1)
			(let ((folded-arg (fold (car args))))
				(cond ((symbol? folded-arg) `(string? ,folded-arg))
					  ((and (list? folded-arg) (eq? (car folded-arg) 'string-append)) #t)
					  (else (string? folded-arg))))
		(error 'number? "incorrect number of arguments"))))

;;;;;;;;;;;;;;;;;;;; APPEND ;;;;;;;;;;;;;;;;;;;;
		
(define fold-append
	(lambda (args)
		(let* ((folded-args (map fold args))
			   (list-args (map cadr (filter quote-list? folded-args)))
			   (symbol-args (filter symbol? folded-args))
			   (invalid-args (remp symbol? (remp quote-list? folded-args))))
			(cond ((not (null? invalid-args))
							(error 'append (format "invalid arguments: ~s" invalid-args)))
				  ((null? symbol-args) `',(apply append list-args))
				  ((null? list-args) `(append ,@symbol-args))
				  (else `(append ,@symbol-args ',(apply append list-args)))))))
				  
;;;;;;;;;;;;;;;;;;;; STRING-APPEND ;;;;;;;;;;;;;;;;;;;;
		
(define empty-string?
	(lambda (s)
		(equal? s "")))
		
;returns #t if there are at least 2 strings next to each others
(define strings-left-to-fold?
		(lambda(lst)
		     ;if the list has at least 2 elements and the first 2 are strings: return #t
			(if (and (not (null? lst)) (>= (length lst) 2) (string? (car lst)) (string? (cadr lst)))
			     #t
			     (if (>= (length lst) 2) (strings-left-to-fold? (cdr lst)) #f) 
				 )))
		
;returns #t if the given list consists only of symbols and strings, else #f
(define Symbols-Strings-List?
		(lambda(lst)
			(if (list? lst)
				(and (not (null? (filter symbol? lst))) ;at least one symbol
					 (> (length (filter string? lst)) 1) ;at least 2 strings
					 (strings-left-to-fold? lst)
					 ;all arguments are either of type symbol or string
					 (null? (remp symbol? (remp string? lst))))
				#f)))

;transforms each 2 strings that are next to each other to one string, 
;deep-string-fold("a" "b" x y "c" "d" "e" "f") -> '("ab" x y "cd" "ef")
(define deep-string-fold
		(lambda(args)
			(if (null? args)
				'()
				(cond
					((and (>= (length args) 2) (string? (car args)) (string? (cadr args)))
							(append (list (string-append (car args) (cadr args))) (deep-string-fold (cddr args))))
					(else (append (list (car args)) (deep-string-fold (cdr args))))))))

;uses deep-string-fold to return a folded version of args
(define rec-deep-string-fold
		(lambda(args)
				;if there are still strings to fold
				(if (strings-left-to-fold? args) 
					(rec-deep-string-fold (deep-string-fold args))
					args)))
				  
(define string-append?
	(lambda (expr)
		(and (list? expr) (equal? (car expr) 'string-append))))
				  
(define fold-string-append
	(lambda (args)
		(let* ((folded-args (remp empty-string? (map fold args)))
			   (string-args (filter string? folded-args))
			   (symbol-args (filter symbol? folded-args))
			   (invalid-args (remp string-append? (remp symbol? (remp string? folded-args)))))
			(cond ((not (null? invalid-args))
							(error 'string-append (format "invalid arguments: ~s" invalid-args)))
				  ((null? string-args) `(string-append ,@folded-args)) ; no strings in args
				  ((null? (remp string? folded-args)) (apply string-append string-args)) ;only strings in args
				  ((and (Symbols-Strings-List? folded-args) (strings-left-to-fold? folded-args))
						`(string-append ,@(rec-deep-string-fold folded-args))) ;symbols (at least one) and strings left to append
				  (else `(string-append ,@folded-args)))))) ;no more folding can be done

;;;;;;;;;;;;;;;;;;;; IF ;;;;;;;;;;;;;;;;;;;;
				  
(define var?
	(lambda (e)
		(if (symbol? e)
			(not (contains? (list 'quote '+ '* 'zero? 'add1 'sub1 'list 'cons 'null? 
							'number? 'string? 'string-append 'car 'cdr 'append 'if) e))
			#f)))
				
(define has-var?
	(lambda (expr)
		(if (var? expr)
			#t
			(if (and (list? expr) (not (null? expr)))
				(or (var? (car expr)) (contains? (map has-var? (cdr expr)) #t))
				#f))))
				
(define fold-if
	(lambda (test dit dif)
		(cond ((equal? dit dif) dit)
			  ((has-var? test) (if (eq? dif (make-void))
								   `(if ,test ,dit)
								   `(if ,test ,dit ,dif)))
			  (else (if (get-quoted-e test)
						(get-quoted-e dit)
						(get-quoted-e dif))))))

;;;;;;;;;;;;;;;;;;;; CAR / CDR ;;;;;;;;;;;;;;;;;;;;
			
(define fold-car-cdr
	(lambda (func func-sym expr)
		(let ((folded-expr (get-quoted-e (fold expr))))
			(if (or (list? folded-expr) (pair? folded-expr))
				(cond ((equal? (car folded-expr) 'cons) (fold (car (func (cdr folded-expr)))))
					  ((equal? (car folded-expr) 'list) (fold (func (cdr folded-expr))))
					  (else (fold (func folded-expr))))
				`(,func-sym ,folded-expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				
(define foo
	(lambda (n s)
		(if (zero? n)
			s
			(let ((w (foo (- n 1) s)))
				(append
					(map (lambda (wi) `(car ,wi)) w)
					(map (lambda (wi) `(cdr ,wi)) w))))))

(define goo
	(lambda (n s)
		(if (zero? n)
			'(())
			(let ((w (goo (- n 1) s)))
				(append (map (lambda (wi) `(#\a ,@wi)) w)
						(map (lambda (wi) `(#\d ,@wi)) w))))))

(define boo
	(lambda (s)
		(map (lambda (si) (string->symbol (format "c~ar" (list->string si)))) 
			s)))

; create pattern rules for all combinations of car and cdr
(define create-car-cdr-pattern-rules
	(lambda ()
		(let* ((s '(4 3 2 1))
			   (s1 (apply append (map (lambda (i) (foo i '(,(? 'expr)))) s)))
			   (s2 (apply append (map (lambda (i) (boo (goo i '()))) s))))
			(map (lambda (s1i s2i)
						`(pattern-rule
							,(list 'quasiquote s1i)
							(lambda (expr) (fold-car-cdr ,s2i ' ,s2i expr))))
						s1 s2))))
; ==>  ((pattern-rule
;			`(car (car (car (car ,(? 'expr)))))
;   		(lambda (expr) (fold-car-cdr caaaar 'caaaar expr)))
; 		(pattern-rule
;   		`(car (car (car (cdr ,(? 'expr)))))
;   		(lambda (expr) (fold-car-cdr caaadr 'caaadr expr)))
;		.
;		.
;		.
;		(pattern-rule
;			`(car ,(? 'expr))
;			(lambda (expr) (fold-car-cdr car 'car expr)))
;		(pattern-rule
;			`(cdr ,(? 'expr))
;		   	(lambda (expr) (fold-car-cdr cdr 'cdr expr))))

