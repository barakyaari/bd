(load "pattern-matcher.scm")
; --------------------------- Helpers ------------------------------

(define simpleConstant?
  (lambda (sexpr)
    (if (or 
          (char? sexpr)
          (string? sexpr)
          (number? sexpr)
          (boolean? sexpr))
        #t
        #f)))



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

;Vector:
   (pattern-rule
      (? 'v vector?)
     (lambda (sexpr) `(const ,sexpr)))




     )))

;Final:
 (lambda (sexpr)
 (run sexpr (lambda ()
                (error 'tag-parse 
                       (format "Failed to parse input: ~s" sexpr)))))))


(tag-parse '#(1 (1 2 3 4) 2))