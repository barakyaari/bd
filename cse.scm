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

    (andmap (lambda (x)
              (const? x))
              expr)
  )))

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
        (generateListOfPairsAndExpression (cons (append pairs (list pair)) (swapInList toSwap generated body))))
    (cons pairs body)))))

(define cse2
  (lambda (exp)
    (let* (
           (pair (generateListOfPairsAndExpression (cons '() exp)))
           (body (cdr pair))
           (pairs  (car pair)))
      (if (null? pairs)
          exp
      (if (equal? (length pairs) 1)
          
      `(let
         ,pairs
         ,body)
          
      `(let*
         ,pairs
         ,body))))))


(cse2 '(list '(a b)
(list '(a b) '(c d))
(list '(a b) '(c d))))

