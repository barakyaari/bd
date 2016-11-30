(load "pattern-matcher.scm")
; --------------------------- Helpers ------------------------------

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

;Quotes sexpr:

   (pattern-rule
     `(quote ,(? 'sexpr))
     (lambda (sexpr) `("hello!")))

    
;Vector:
   (pattern-rule
      (? 'v vector?)
     (lambda (sexpr) `(const ,sexpr)))
   
; empty begin:

  (pattern-rule
    `(,(? 'exp begin?))
    (lambda (exp) `(const ,(createVoid))))
   
   
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
        (if (and (null? more-expressions)
                 (not (list? more-expressions)))
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
  
; And:
  (pattern-rule
    `(and . ,(? 'expressions))
       (lambda (expressions) (tag-parse (expandAnd expressions))))

  
  
  
  







;Final:
    )))
 (lambda (sexpr)
 (run sexpr (lambda ()
                (error 'tag-parse 
                       (format "Failed to parse input: ~s" sexpr)))))))






; -------------------------- Macro-Expansions ----------------------



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

 (tag-parse '(and (= 1 1) (= 3 3) (= 1 1)))
