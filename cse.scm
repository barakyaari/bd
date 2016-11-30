(load "pattern-matcher.scm")

(define contains
	(lambda (lst item)
		(member item lst)
			))

(define isValidList?
	(lambda (lista)
		(if (null? lista)
			#t
			(and (not (contains (cdr lista) (car lista)))
				 (isValidList? (cdr lista))))))


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
         (if (isListOfLists? body))
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


(cse '(+ (* (- x y) (* x x))
(* x x)
(foo (- x y))
(goo (* (- x y) (* x x)))))
