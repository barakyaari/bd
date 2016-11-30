(load "pattern-matcher.scm")
; --------------------------- Helpers ------------------------------
; -- meirs expand-qq - check if usable:
(define expand-qq
  (lambda (e)
    (cond ((unquote? e) (cadr e))
    ((unquote-splicing? e) (error 'expand-qq "unquote-splicing here makes no sense!"))
    ((pair? e)
     (let ((a (car e))
     (b (cdr e)))
       (cond ((unquote-splicing? a) `(append ,(cadr a) ,(expand-qq b)))
       ((unquote-splicing? b) `(cons ,(expand-qq a) ,(cadr b)))
       (else `(cons ,(expand-qq a) ,(expand-qq b))))))
    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
    ((or (null? e) (symbol? e)) `',e)
    (else e))))

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
     (eq? (car e) tag)
     (pair? (cdr e))
     (null? (cddr e))))))

(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))



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
    (if (and (null? lista)
             )
      #t
      (and (not (member (car lista) (cdr lista)))
         (isValidList? (cdr lista))))))

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
    `(begin ,(? 'sexpr) . ,(? 'otherSexpr))
    (lambda (sexpr otherSexpr)
      `(seq (,(tag-parse sexpr) ,@(map tag-parse otherSexpr)))))
  ;set:
  (pattern-rule
    `(set! ,(? 'sexpr) . ,(? 'otherSexpr))
    (lambda (sexpr otherSexpr)
      `(seq (,(tag-parse sexpr) ,@(map tag-parse otherSexpr)))))

   
; -------------- Disjunctions: --------------

; Or:
   (pattern-rule
      `(or . ,(? 'orExpressions))
     (lambda (orExpressions)
       `(or ,(map tag-parse orExpressions)))) 
   
; -------------- Lambda: --------------

; Lambda:
  (pattern-rule
    `(lambda 
       ,(? 'parameters)
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
            `(lambda-variadic ,parameters
              ,(tag-parse newExpressions)))))))
  
  
  
  ; -------------- Define: --------------
  
  (pattern-rule
    `(define ,(? 'variable) ,(? 'value))
    (lambda (variable value)
      (if (symbol? variable)
        `(define ,(tag-parse variable) ,(tag-parse value))   
          ; Mit define:
        (tag-parse (expandMitDefine variable value)))))
        
        

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
  
; Letrec:
  (pattern-rule
    `(letrec ,(? 'listOfVarsAndVals) . ,(? 'expressions))
    (lambda (listOfVarsAndVals expressions)
      (tag-parse (expandLetrec listOfVarsAndVals (car expressions)))))
  
; And:
  (pattern-rule
    `(and . ,(? 'expressions))
       (lambda (expressions) (tag-parse (expandAnd expressions))))

; Cond:
(pattern-rule
  `(cond ,(? 'first) . ,(? 'others))
  (lambda (first others)
    (tag-parse (expandCond first others))))






; Quasiquote:
(pattern-rule
  `(quasiquote . ,(? 'expression))
  (lambda (expression)
    (if (and 
          (not (null? expression))
          (= (length expression) 1)
          )
      (expand-qq (car expression))
      (error 'quasiqoute "quasiquote got wrong parameters"))))

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
        `(letrecFunction ,bodyFunction ,@afterApplies)
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
    (if (or (null? others) (and (list? others) (null? others)))
      `(let ((,variable ,value)) . ,expressions)
      `(let ((,variable ,value)) (let* ,others . ,expressions)))))
        
(define expandEmptyLet*
  (lambda (expressions)
    (if (and (list? expressions)
             (null? expressions)
             )
        (error 'letrec "empty list - error."))
    `(begin ,@expressions)))

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
        (sexprs (car pairs)) ;for debug, don't forget to delete.
        )
        
      (if (isValidList? variables)
        `((lambda ,variables ,@expressions) ,@values)
        (error 'let "All variables must be different.")))))


(define expandMitDefine
  (lambda (variable value)
    (let ((function (car variable))
        (parameters (cdr variable))
        (vars (cdr variable))
        )
      `(define ,function (lambda ,parameters ,value)))))

(define a 3)

(tag-parse '(* (+ 2 3 4) (+ 2 3 4)))