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


(define *reserved-words*
'(and begin cond define do else if lambda
    let let* letrec or quasiquote unquote
      unquote-splicing quote set!))


(define notReserved?
  (lambda (sexpr)
    (not (member sexpr *reserved-words*))))

(define variable?
  (lambda (sexpr)
    (and (not (member sexpr *reserved-words*))
         (symbol? sexpr)
         )))

(define builder
  (lambda (pairOfBodyAndArgList)
  	    (let ((body (car pairOfBodyAndArgList))
             (argList (cdr pairOfBodyAndArgList))
             )

       body)
    ))

(define basicBuilder
  (lambda (function arguments)

       (builder (cons arguments '()))
    ))



(define cse
 (let 
    ((run
 (compose-patterns

   ; Application:
     (pattern-rule
    `(,(? 'function
          (lambda (reserved?)
               (not (member reserved? *reserved-words*))))
          . ,(? 'arguments))
    (lambda (function arguments)
      (basicBuilder function arguments)
      ))

      
      
          )))
 (lambda (sexpr)
 (run sexpr (lambda ()
                (error 'tag-parse 
                       (format "Failed to parse input: ~s" sexpr)))))))


(define isSimpleList
  (lambda (expr)
    (if (not (list? expr))
        #f
        (if (= (length expr) 1)
            #f
    (andmap (lambda (x)
              (not (list? x)))
              expr)
  ))))


(define getSimpleLists
  (lambda (expr)
    (if (not (list? expr))
        '()
    (if (isSimpleList expr)
        (if (null? expr)
            '()
        `(,expr))
        
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
        (let* ((generated (gensym))
              (toSwap (getFirstDoubleSimpleList body))
              (pair `(,toSwap ,generated))
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
         (,pairs)
         ,body))))


(cse '(list (cons 'a 'b)
(cons 'a 'b)
(list (cons 'a 'b)
(cons 'a 'b))
(list (list (cons 'a 'b)
(cons 'a 'b)))))


 

 
