(load "pattern-matcher.scm")


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

(define filterOut
  (lambda (toFilter lista)
    (filter (lambda (x)
             (equal? x toFilter)) lista)
            ))

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
 
(define filterOutExpressionFromList
  (lambda (toFilterOut exp)
    (swapInList toFilterOut 'bobcat exp)))

(define isAppearingByItselfInFullExpression
  (lambda (toCheck largerExpression OriginalExpression)
    (isSublist toCheck (filterOutExpressionFromList largerExpression OriginalExpression))))

(define isSecondAppearingTwiceInFirst
  (lambda (first second)
    (let* (
           (listWithSwappedSecond (swapInList second 'bobcat first))
           (flattendedFirst (flatten listWithSwappedSecond))
           (listOfSwapped (filterOut 'bobcat flattendedFirst))        
             )
      (> (length listOfSwapped) 1))))

 (define isSecondBiggerThanFirst
   (lambda (originalExpression)
   (lambda (first second)
     (let ((firstIsByItself (isAppearingByItselfInFullExpression first second originalExpression))
           (firstIsSublistOfSecond (isSublistOfItem first second))
           (isSecondAppearingTwice (isSecondAppearingTwiceInFirst first second))
           (isFirstAppearingTwice (isSecondAppearingTwiceInFirst second first))
           )
       (if isSecondAppearingTwice
           #t
       (if isFirstAppearingTwice
          #t
     (if firstIsByItself
      #t
      (if (isSublistOfItem first second)
        #f
      (> (length (flatten first)) (length (flatten second)))))))))))

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
    (cond ((not (list? x)) #t)
          ((null? x) #t)
          ((equal? (car x) 'quote) #t)
          (else #f)
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
      `( ,@(if (not (const? (car expr)))
            (list (car expr))
              '())
             ,@(getAllListsInExpression (car expr))
              ,@(getAllListsInExpression (cdr expr))
    )))
  )))

(define getOrderedLists
  (lambda (expr originalExpression)
    (display (list-sort (isSecondBiggerThanFirst originalExpression) (getAllListsInExpression expr)))
    (newline)
        (newline)

    (list-sort (isSecondBiggerThanFirst originalExpression) (getAllListsInExpression expr))))

(define getFirstDoubleList
  (lambda (expr originalExpression)
    (containsDouble (getOrderedLists expr originalExpression))))


(define hasDoubleList
  (lambda (expr originalExpression)
    (if (= (length (getFirstDoubleList expr originalExpression)) 0)
    #f
    #t)))

(define generateListOfPairsAndExpression
  (lambda (pairOfPairListAndExpression originalExpression)
    
  (let ((pairs (car pairOfPairListAndExpression))
        (body (cdr pairOfPairListAndExpression))
        )
    (if (hasDoubleList body originalExpression)
        (let* ((generated (gensym))
              (toSwap (getFirstDoubleList body originalExpression))
              (pair (list generated (car (list toSwap))))
              )
        (generateListOfPairsAndExpression (cons (append pairs (list pair)) (swapInList toSwap generated body)) originalExpression))
    (cons pairs body)))))

(define cse2
  (lambda (exp)
    (let* (
           (pair (generateListOfPairsAndExpression (cons '() exp) exp))
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

(define toTest '(* (+ (+ 1 (+ 2 (- 3 (+ 4 5))))) (+ (+ 1 (+ 2 (- 3 (+ 4 5))))) (+ (+ 11 (+ 22 (- 45 (+ 4 5))))) (+ (+ 113 (+ 220 (- 3 (+ 4 5)))))))
(getOrderedLists toTest toTest)

(cse2 toTest)


            
  