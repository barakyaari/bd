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

(define cse
  (lambda (args)
    (if (isValidList? args)
        args
        "working"
    )))

(cse '(* (+ 2 3 4) (+ 2 3 4)))