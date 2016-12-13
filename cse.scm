(load "pattern-matcher.scm")

(define isSublistOfItem
  (lambda (exp toCheck)
    (if (equal? exp toCheck)
    #t
    (if (list? toCheck)
      (or 
        (if (list? (car toCheck))
                   (isSublistOfItem exp (car toCheck))
                   #f)
        (if (not (null? (cdr toCheck)))
            (isSublistOfItem exp (cdr toCheck))
            #f))
      #f
        )))
  )

(define isSublist
  (lambda (exp lista)
    (ormap 
      (lambda (x)
        (isSublistOfItem exp x)) lista)))


(define getFirstNonSublist
  (lambda (lista)
    (if (not (isSublist (car lista)
                   (cdr lista)))
    (car lista)
    (getFirstNonSublist (cdr lista)))))

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

(define getLists2
  (lambda (expr)
    (if (not (list? expr))
        '()
        (if (null? expr)
            '()
            (if (const? expr)
                '()
      `( ,@(if (list? (car expr))
            (list (car expr))
              '())
             ,@(getLists2 (car expr))
              ,@(getLists2 (cdr expr))
    )))
  )))

(define getDoubleLists
  (lambda (expr)
    (sortLst (getLists2 expr))))

(define getFirstDoubleSimpleList
  (lambda (expr)
    (containsDouble? (getLists2 expr))))

(define swapInList
  (lambda (old new lista)
    (if 
      (not (list? lista))
        lista
        (if (const? lista)
            lista
        (if (equal? lista old)
             new
        (if (null? lista)
            lista
        (cons (swapInList old new (car lista))
              (swapInList old new (cdr lista)))))))))

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

(define getLists
  (lambda (expr)
    (if (not (list? expr))
        '()
    (if (isSimpleList expr)
        (if (null? expr)
            '()
            (if (equal? (car expr) 'quote)
              '()
            
        `(,expr)))
    
      `(,expr ,@(getLists (car expr))
              ,@(getLists (if (= (length (cdr expr)) 1)
                              (car (cdr expr))
                              (cdr expr))
    ))
  ))))


            

;(containsDouble? (getLists2 '((+ 1 2) (+ 2 55) (+ 1 2 (+ 2 3) (+ 1 1)) (+ 1 2 (+ 2 3) (+ 1 1)))))
  

(getLists2      '('(' '(a b) '(a b))   
      ))


;(load "cse.so")
;(cse '(f '('(+ x 1)) (f x) '(+ x 1)))


