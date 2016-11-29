;;; qq.scm
;;; A naive, one-level quasiquote implementation
;;;
;;; Programmer: Mayer Goldberg, 2014

;;; expand-qq assumes that you are passing to it the quasiquoted expression
;;; in other words, the expression without the quasiquote!
;;;
;;; Try:
;;; 
;;; > (expand-qq '(a b c ,d e f ,@(foo x y) ,g h i j))

(define expand-qq
	(lambda (e)
		(cond ((unquote? e) (cadr e))
			  ((unquote-splicing? e)
					(error 'expand-qq "unquote-splicing here makes no sense!"))
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

