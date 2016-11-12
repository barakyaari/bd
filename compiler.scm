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

done))

(define <VisibleSimpleChar>
  (new 
  	 (*parser (range #\! #\~))
done))

(define <NamedChar>
  (new 
       (*parser (word "lambda"))
       (*parser (word "newline"))
       (*parser (word "nul"))
       (*parser (word "page"))
       (*parser (word "return"))
       (*parser (word "space"))
       (*parser (word "tab"))
       (*disj 7)
done))

(test-string <NamedChar> "tab")