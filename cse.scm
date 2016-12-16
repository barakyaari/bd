(load "pattern-matcher.scm")

  (define flatten
   (lambda (lista)
     (if (not (list? lista))
         '()
         (if (null? lista)
             '()
             (append
             (if (list? (car lista))
                 (flatten (car lista))
                 (list (car lista)))
                 (flatten (cdr lista))
                           )
                        )
             )))
 
 (define isBigger
   (lambda (x y)
     (> (length (flatten x)) (length (flatten y))))) 

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

(define containsDouble
  (lambda (lista)
    (if (null? lista)
      `()
        (if (not (list? (car lista)))
              (containsDouble (cdr lista))
      (if (contains (cdr lista) (car lista))
            (car lista)
         (containsDouble (cdr lista)))))))

(define const?
  (lambda (x)
    (cond ((not (list? x)) #f)
          ((null? x) #f)
          ((equal? (car x) 'quote) #f)
          (else #t)
          )
   ))

(define isSimpleList
  (lambda (expr)
    (if (not (list? expr))
        #f

    (andmap (lambda (x)
              (const? x))
              expr)
  )))

(define getSimpleListsOld
  (lambda (expr)
    (if (not (list? expr))
        '()
    (if (isSimpleList expr)
        (if (null? expr)
            '()
            (if (equal? (car expr) 'quote)
              '()
            
        `(,expr)))
        
        `( ,@(getSimpleListsOld (car expr))
              ,@(getSimpleListsOld (cdr expr)))
        ))
  ))

(define getAllListsInExpression
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
             ,@(getAllListsInExpression (car expr))
              ,@(getAllListsInExpression (cdr expr))
    )))
  )))

(define getOrderedLists
  (lambda (expr)
    (list-sort isBigger (getAllListsInExpression expr))))

(define getFirstDoubleList
  (lambda (expr)
    (containsDouble (getOrderedLists expr))))

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

(define hasDoubleList
  (lambda (expr)
    (if (= (length (getFirstDoubleList expr)) 0)
    #f
    #t)))

(define generateListOfPairsAndExpression
  (lambda (pairOfPairListAndExpression)
    
  (let ((pairs (car pairOfPairListAndExpression))
        (body (cdr pairOfPairListAndExpression))
        )
    (if (hasDoubleList body)
        (let* ((generated (gensym))
              (toSwap (getFirstDoubleList body))
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




            

;(containsDouble? (getLists2 '((+ 1 2) (+ 2 55) (+ 1 2 (+ 2 3) (+ 1 1)) (+ 1 2 (+ 2 3) (+ 1 1)))))

(getAllListsInExpression '(* (+ 1 2) (+ 1 2)))

;(const? '(a b))

(load "cse.so")
(cse  '(* (+ 1 2) (+ 1 2)))


