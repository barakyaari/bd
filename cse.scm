(load "pattern-matcher.scm")

(define contains
	(lambda (lst item)
		(member item lst)
			))

(define containsDouble?
	(lambda (lista)
		(if (null? lista)
			`()
   			(if (not (list? (car lista)))
          		(containsDouble? (cdr lista))
			(if (contains (cdr lista) (car lista))
       			(car lista)
				 (containsDouble? (cdr lista)))))))

(define const?
  (lambda (x)
    (if (list? x)
        (if (> (length x) 1)
            (if (equal? (car x) 'quote)
            	#t
            	#f)
           	#f)
          #t)
    ))

(define isSimpleList
  (lambda (expr)
    (if (not (list? expr))
        #f
        (if (= (length expr) 1)
            #f
    (andmap (lambda (x)
              (const? x))
              expr)
  ))))

(define isQuoted
  (lambda (arg)
    #t))

(define getSimpleLists
  (lambda (expr)
    (if (not (list? expr))
        '()
    (if (isSimpleList expr)
        (if (null? expr)
            '()
            (if (equal? (car expr) 'quote)
              '()
            
        `(,expr)))
        
        `( ,@(getSimpleLists (car expr))
              ,@(getSimpleLists (cdr expr)))
        ))
  ))

(define getFirstDoubleSimpleList
  (lambda (expr)
    (containsDouble? (getSimpleLists expr))))

(define swapInList
  (lambda (old new lista)
    (if 
      (not (list? lista))
        lista
        (if (equal? lista old)
             new
        (if (null? lista)
            lista
        (cons (swapInList old new (car lista))
              (swapInList old new (cdr lista))))))))

(define hasDoubleSimpleList
  (lambda (expr)
    (if (= (length (getFirstDoubleSimpleList expr)) 0)
    #f
    #t)))

(define generateListOfPairsAndExpression
  (lambda (pairOfPairListAndExpression)
    
  (let ((pairs (car pairOfPairListAndExpression))
        (body (cdr pairOfPairListAndExpression))
        )
    (if (hasDoubleSimpleList body)
        (let* ((generated (string->symbol (symbol->string (gensym))))
              (toSwap (getFirstDoubleSimpleList body))
              (pair (list generated (car (list toSwap))))
              )
        (generateListOfPairsAndExpression (cons (append pairs pair) (swapInList toSwap generated body))))
    (cons pairs body)))))

(define cse
  (lambda (exp)
    (let* (
           (pair (generateListOfPairsAndExpression (cons '() exp)))
           (body (cdr pair))
           (pairs  (car pair)))
      
      `(let*
         ,pairs
         ,body))))


 (newline)
 (cse '(list (cons 'a 'b)
(cons 'a 'b)
(list (cons 'a 'b)
(cons 'a 'b))
(list (list (cons 'a 'b)
(cons 'a 'b)))))

(newline)


(define lista '('a (a b c) c))
(define term (car (cdr lista)))
term
(length term)
(> (length term) 1)
(const? term)
(string->symbol (symbol->string (gensym)))