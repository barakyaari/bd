(load "pc.scm")
;; --------------------------------
;;           Essentials:
;; --------------------------------

(define <hex-digit>
  (let ((zero (char->integer #\0))
  (lc-a (char->integer #\a))
  (uc-a (char->integer #\A)))
    (new (*parser (range #\0 #\9))
   (*pack
    (lambda (ch)
      (- (char->integer ch) zero)))

   (*parser (range #\a #\f))
   (*pack
    (lambda (ch)
      (+ 10 (- (char->integer ch) lc-a))))

   (*parser (range #\A #\F))
   (*pack
    (lambda (ch)
      (+ 10 (- (char->integer ch) uc-a))))

   (*disj 3)
   done)))

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
        (lambda(a b)
          `(#\#  #\\)))

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
       (*pack
        (lambda(_)
          `(,@_)))
done))

(define <HexUnicodeChar>
  (new 
       (*parser <hex-digit>)
       (*parser <hex-digit>) *star
       (*caten 2)
done))

(define <Char>
  (new 
       (*parser <CharPrefix>)

       (*parser <NamedChar>)
       (*parser <VisibleSimpleChar>)
       (*parser <HexUnicodeChar>)
       (*disj 3)

       (*caten 2)
       (*pack-with
        (lambda(a b)
          (list->string `(,@a ,@b))))
      
done))

(test-string <Char> "#\\space")