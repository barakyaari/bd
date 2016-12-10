(load "pattern-matcher.scm")
; --------------------------- Helpers ------------------------------

(define makeBegin
  (lambda (exp)
    (cond
      ((null? exp) (void))
      ((null? (cdr exp)) (car exp))
      (else `(begin ,@exp)))))

; ---------------Meirs Expand-qq-------------------
(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
     (eq? (car e) tag)
     (pair? (cdr e))
     (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
   (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
     simple-sexprs-predicates)
    (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
      (pair? e)
      (symbol? e)
      (vector? e))
  `',e
  e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
  (cadr e)
  e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
   (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
      (lambda (e)
        (cond ((unquote? e) (cadr e))
        ((unquote-splicing? e)
         (error 'expand-qq
           "unquote-splicing here makes no sense!"))
        ((pair? e)
         (let ((a (car e))
         (b (cdr e)))
           (cond ((unquote-splicing? a)
            `(append ,(cadr a) ,(expand-qq b)))
           ((unquote-splicing? b)
            `(cons ,(expand-qq a) ,(cadr b)))
           (else `(cons ,(expand-qq a) ,(expand-qq b))))))
        ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
        ((or (null? e) (symbol? e)) `',e)
        (else e))))
     (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
     (optimizer
      (compose-patterns
       (pattern-rule
        `(append ,(? 'e) '())
        (lambda (e) (optimize-qq-expansion e)))
       (pattern-rule
        `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
        (lambda (c1 c2 e)
    (let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
          (e (optimize-qq-expansion e)))
      (optimize-qq-expansion `(append ,c ,e)))))
       (pattern-rule
        `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
        (lambda (c1 c2)
    (let ((c (quotify (append (unquotify c1) (unquotify c2)))))
      c)))
       (pattern-rule
        `(append ,(? 'e1) ,(? 'e2))
        (lambda (e1 e2)
    (let ((e1 (optimize-qq-expansion e1))
          (e2 (optimize-qq-expansion e2)))
      `(append ,e1 ,e2))))
       (pattern-rule
        `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
        (lambda (c1 c2 e)
    (let ((c (quotify (list (unquotify c1) (unquotify c2))))
          (e (optimize-qq-expansion e)))
      (optimize-qq-expansion `(append ,c ,e)))))
       (pattern-rule
        `(cons ,(? 'e1) ,(? 'e2))
        (lambda (e1 e2)
    (let ((e1 (optimize-qq-expansion e1))
          (e2 (optimize-qq-expansion e2)))
      (if (and (const? e1) (const? e2))
          (quotify (cons (unquotify e1) (unquotify e2)))
          `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))

(define makeBegin
  (lambda (exp)
    (cond
      ((null? (cdr exp)) (car exp))
      ((null? exp) *void-object*)
      (else 
        `(begin ,@exp)))))

(define simpleConstant?
  (lambda (sexpr)
    (or 
          (char? sexpr)
          (string? sexpr)
          (number? sexpr)
          (boolean? sexpr))
        ))

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

(define createVoid
  (lambda ()
    (if #f #f)))

(define getRestOfParameters
  (lambda (params)
    (if       
      (and 
        (not (null? params)) (pair? params))
      (getRestOfParameters (cdr params))
      params)))

(define getRequiredParameters
  (lambda (params)
    (if 
      (and 
        (not (null? params)) (pair? params))
        
      (append (list (car params)) (getRequiredParameters (cdr params)))
      '())))

(define begin?
  (lambda (expression)
    (eq? expression 'begin)))
      
      
(define isValidList?
  (lambda (lista)
    (if(null? lista) 
        #t
        (if (not (list? lista))
            #t
      (and (not (member (car lista) (cdr lista)))
         (isValidList? (cdr lista)))))))

(define getValues
  (lambda (lista)
    (map cadr lista)))

(define getVariables
  (lambda (variable lista)
    (let ((variables (cons variable (map car lista))))
      (if (not (isValidList? variables))
         (error 'let "Variables must all be different")
        variables))))

(define isLetVariableList?
  (lambda (lista)
    (and (list? lista) (andmap 
      (lambda (values) (and (equal? (length values) 2) (pair? values)))
       lista))))

(define swapListItem
  (lambda (exp old new)
    (if (list? exp)
        (if (null? exp)
            '()
            (append (list (swapListItem (car exp) old new))
                    (swapListItem (cdr exp) old new)
                    )
            )
        (if (equal? exp old)
            new
            exp)
          )))

(define contains?
  (lambda (exp arg)
    (if (list? exp)
      (ormap (lambda (x)
             (equal? x arg))
           exp
             )
      #f)))
          
  (define parseInnerBegin
    (lambda (expressions)
      (let ((exp (substq 'innerBegin 'begin expressions))
            (applier (lambda (x)
                (tag-parse x))))
         (map applier exp)
     )))
  
  (define expressionWithoutSeq
    (lambda (expression)
      (if (null? expression)
        '()
        (if
          (and (list? (car expression))
                 (equal? 'seq (car (car expression))))
          `(,@(car (cdr(car expression)))
               ,@(expressionWithoutSeq (cdr expression)))
          `(,(car expression) ,@(expressionWithoutSeq (cdr expression))))
          )
      ))

; ------------------------ Tag parser --------------------------------

(define tag-parse
 (let 
    ((run
 (compose-patterns

   ;Null:
   (pattern-rule
     (? 'sexpr null?)
     (lambda (sexpr) '()))
; Number, Boolean, String, Char:   
      (pattern-rule
     (? 'sexpr simpleConstant?)
     (lambda (sexpr) `(const ,sexpr)))
; empty begin:
  (pattern-rule
    `(,(? 'exp begin?))
    (lambda (exp) `(const ,(createVoid))))
;Quotes sexpr:

   (pattern-rule
     `(quote ,(? 'sexpr))
     (lambda (sexpr) `(const ,sexpr)))
    
;Vector:
   (pattern-rule
      (? 'v vector?)
     (lambda (sexpr) `(const ,sexpr)))
      
; -------------- Variables: --------------
   (pattern-rule
      (? 'v variable?)
     (lambda (sexpr) `(var ,sexpr)))
   
; -------------- Conditionals: --------------

; If with no Do if false:
   (pattern-rule
      `(if ,(? 'test) ,(? 'doIfTrue))
     (lambda (test doIfTrue)
       `(if3 ,(tag-parse test) ,(tag-parse doIfTrue)
         (const ,(createVoid))))) 

   ; If with Do if false:
      (pattern-rule
      `(if ,(? 'test) ,(? 'doIfTrue) ,(? 'doIfFalse))
     (lambda (test doIfTrue doIfFalse)
       `(if3 ,(tag-parse test) ,(tag-parse doIfTrue)
         ,(tag-parse doIfFalse)))) 
      
        ; -------------- Sequences: --------------
    ;begin:
    (pattern-rule
    `(innerBegin ,(? 'sexpr) . ,(? 'otherSexpr))
    (lambda (sexpr otherSexpr)
      (if (null? otherSexpr)
      (tag-parse sexpr)
          (let* (
                 (expression (tag-parse sexpr))
                 (others (map tag-parse otherSexpr))
                 )
        (cons expression others))
                   )))
    
  (pattern-rule
    `(begin . ,(? 'others))
    (lambda (others) 
      (cond ((null? others) 
             `(const ,*void-object*))
          ((equal? 1 (length others)) 
           (tag-parse (car others)))
          (else 
            (let ((othersFixed others))
                `(seq ,(expressionWithoutSeq (map tag-parse othersFixed))))))))

  ;set:
  (pattern-rule
    `(set! ,(? 'sexpr) . ,(? 'otherSexpr))
    (lambda (sexpr otherSexpr)
      `(set ,(tag-parse sexpr) ,@(map tag-parse otherSexpr))))
   
; -------------- Disjunctions: --------------

; Or:
   (pattern-rule
      `(or . ,(? 'orExpressions))
     (lambda (orExpressions)
       (if (null? orExpressions)
           `(const #f)
           (if(= (length orExpressions) 1)
           `,(tag-parse (car orExpressions))
       `(or ,(map tag-parse orExpressions)))))) 
   
; -------------- Lambda: --------------

; Lambda:
  (pattern-rule
    `(lambda 
       ,(? 'parameters isValidList?)
       ,(? 'expression) . ,(? 'more-expressions))
    (lambda (parameters expression more-expressions)
      (let ((newExpressions '()))
        (if (null? more-expressions)
                
          (set! newExpressions expression)
          (set! newExpressions
                (append (list 'begin)
                   (list expression) more-expressions)))
        (cond ((list? parameters)
                   `(lambda-simple ,parameters
              ,(tag-parse newExpressions)))
            ((pair? parameters)
            `(lambda-opt ,(getRequiredParameters parameters) ,(getRestOfParameters parameters)
              ,(tag-parse newExpressions)))
            (else
            `(lambda-var ,parameters
              ,(tag-parse newExpressions)))))))
  
  ; -------------- Define: --------------
  (pattern-rule
    `(define ,(? 'variables)  ,(? 'expressions) . ,(? 'others))
    (lambda (variables expressions others) 
    
    (if 
      (and (pair? variables) 
           #t)
       (tag-parse (expandMitDefine variables (makeBegin (cons expressions others))))
       
      `(def ,(tag-parse variables) ,(tag-parse (makeBegin (cons expressions others))))))
  )
  
  ; application
  (pattern-rule
    `(,(? 'function 
          (lambda (reserved?)
               (not (member reserved? *reserved-words*))))
          . ,(? 'arguments))
    (lambda (function arguments)
      `(applic ,(tag-parse function) (,@(map tag-parse arguments)))))



; -------------------------- Macro-Expanding-Special-Forms ----------------------
;let:
  (pattern-rule
    `(let ,(? 'pairs) ,(? 'expressions) . ,(? 'moreExpressions))
    (lambda (pairs expressions moreExpressions)
      (tag-parse (expandLet pairs expressions moreExpressions))
      ))
  
    ; let*:
  (pattern-rule
    `(let* () ,(? 'expression)
        . ,(? 'expressions list?))
    (lambda (expression expressions)
      (tag-parse (expandEmptyLet* (cons expression expressions)))))
  (pattern-rule
    `(let* ((,(? 'variable variable?)
                ,(? 'value)) . ,(? 'others)) . ,(? 'expressions))
    (lambda (variable value others expressions)
      (tag-parse (expandLet* variable value others expressions))))
  
  ;Letrec:
  (pattern-rule
    `(letrec ((,(? 'variable variable?) ,(? 'value)) . ,(? 'others isLetVariableList?)) . ,(? 'expressions))
    (lambda (variable value others expressions)
      (let* (
         (emptyList (list))
          (variables (getVariables variable others))
          (values (cons value (getValues others)))
          (pairs (map (lambda (variable2 value2)
                    `(set! ,variable2 ,value2))
                     variables values))
          (nonVars (map (lambda (value) '#f) values)))
        (tag-parse
          `((lambda (,@variables)
            ,(makeBegin 
              `(,@pairs ((lambda () ,@expressions) ,@emptyList))  
            )) 
            ,@nonVars
          )))))
  
;Letrec empty:
  (pattern-rule
    `(letrec ,(? 'vars list? null?) ,(? 'expression) . ,(? 'expressions list?))
      (lambda (variables expression expressions) 
        (tag-parse
          `((lambda ()
              ((lambda () ,(makeBegin (cons expression expressions)))
               ,@variables)) 
            ,@variables)
          )))
  
; And:
  (pattern-rule
    `(and . ,(? 'expressions))
       (lambda (expressions)
         (if (null? expressions)
             `(const #t)
             (tag-parse (expandAnd expressions)))))

; Cond:
(pattern-rule
  `(cond ,(? 'first) . ,(? 'others))
  (lambda (first others)
    (tag-parse (expandCond first others))))

; Quasiquote:
(pattern-rule
  `(quasiquote . ,(? 'expression))
  (lambda (expression)
    `,(tag-parse (expand-qq (car expression)))))

; Un-quote:
(pattern-rule
  `(unquote . ,(? 'expression))
  (lambda (expression) (tag-parse
      (expand-qq expression))))

; Unquote-splicing
(pattern-rule
  `(unquote-splicing ,(? 'expression))
  (lambda (expression) (tag-parse
      (expand-qq expression))))

;Final:
    )))
 (lambda (sexpr)
 (run sexpr (lambda ()
                (error 'tag-parse 
                       (format "Failed to parse input: ~s" sexpr)))))))

; -------------------------- Macro-Expansions ----------------------
(define expandLetrec
  (lambda (lista . expressions)
    (let* ((first (map car lista))
         (lambdaExpressions (map cdr lista))
         (newFirst `(,@first))
         (bodyFunction `(lambda ,newFirst ,@expressions))
         (afterApplies (map (lambda (lambdaExpressions) `(lambda ,newFirst ,@lambdaExpressions)) lambdaExpressions)))
      (if (isValidList? first)
        `(seq (,bodyFunction ,@afterApplies))
        (error 'letrec "All variables must be different.")))))

(define expandLetrec2
  (lambda (lista . expressions)
    (let* ((first (map car lista))
         (lambdaExpressions (map cdr lista))
         (newFirst `(,@first))
         (bodyFunction `(lambda ,newFirst ,@expressions)))
      (if (isValidList? first)
        `(letrecFunction ,bodyFunction)
        (error 'letrec "Test - for debugging.")))))

(define expandLet*
  (lambda (variable value others expressions)
    (if (null? others) 
      `(let ((,variable ,value)) . ,expressions)
      `(let ((,variable ,value)) (let* ,others . ,expressions)))))
        
(define expandEmptyLet*
  (lambda (expressions)
    (display "Parsing empty let\n")
        (display expressions)
    (display "\n")

    `((lambda () ,@expressions))))

  (define expandCond
  (lambda (firstExp restExp)
    (let ((variable1 (car firstExp))
        (expression1 (cdr firstExp)))
      (if (and (> (length expression1) 1) (not (null? expression1)))
        (set! expression1 
              (append (list 'begin) (cdr firstExp)))
        (set! expression1 
              (car expression1)))
      (if (null? restExp)
        (if (eq? variable1 'else)
          expression1
          `(if ,variable1 ,expression1))
        `(if ,variable1 ,expression1 ,(expandCond (car restExp) (cdr restExp)))))))

;Expand and:
(define expandAnd
  (lambda (exressions)
    (cond ((and (null? exressions) (not (list? expressions)))
            '#t)
        ((eq? (length exressions) 1) (car exressions))
        (else `(if ,(car exressions) (and ,@(cdr exressions)) #f)))))

(define expandLet
  (lambda (pairs expressions moreExpressions)
    (let ((variables (map car pairs))
        (values (map cadr pairs))
        (expressions (append (list expressions) moreExpressions))
        )
        
      (if (isValidList? variables)
        `((lambda ,variables ,@expressions) ,@values)
        (error 'let "All variables must be different.")))))

  (define expandMitDefine
    (lambda (variables expressions)
    `(define ,(car variables) 
       (lambda ,(cdr variables) ,expressions))))

  (define value '((begin a (begin b (begin d e f g))) h (i j k)))
  
  (display "expected:\n")
  (display "((var a) (var b) (var d) (var e) (var f) (var g) (var h) (applic (var i) ((var j) (var k))))\n")
(parseInnerBegin
  value
  )