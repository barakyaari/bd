(load "pc.scm")

;; --------------------------------
;;           Boolean:
;; --------------------------------
(define <Boolean>
  (new 
	;; True:
  	(*parser (char #\#))
       (*parser (char #\t))
       (*parser (char #\T))
       (*disj 2)

       (*caten 2)
       (*pack-with
       	(lambda (a b)
	  #t))
	;; False:
  	(*parser (char #\#))
       (*parser (char #\f))
       (*parser (char #\F))
       (*disj 2)

       (*caten 2)
       (*pack-with
       	(lambda (a b)
	  #f))
       (*disj 2)

done))


;; --------------------------------
;;           Char:
;; --------------------------------

(define <CharPrefix>
  (new 
  	(*parser (char #\#))
       (*parser (char #\\))
       (*caten 2)
       (*pack-with
       	(lambda (a b)
	  (list->string `(,a ,b))))
done))

(define <VisibleSimpleChar>
  (new 
  	(*parser (char #\#))
       (*parser (char #\\))
       (*caten 2)
       (*pack-with
       	(lambda (a b)
	  (list->string `(,a ,b))))
done))

(test-string <Boolean> "#F")