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
	  (list->string `(#\# #\t))))
	;; False:
  	(*parser (char #\#))
       (*parser (char #\f))
       (*parser (char #\F))
       (*disj 2)

       (*caten 2)
       (*pack-with
       	(lambda (a b)
	  (list->string `(#\# #\f))))
       (*disj 2)

done))


(test-string <Boolean> "#f")