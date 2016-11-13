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

(define <XX>
  (new (*parser <hex-digit>)
       (*parser <hex-digit>)
       (*caten 2)
       (*pack-with
  (lambda (h l)
    (+ l (* h 16))))
       done))

(define <XXXX>
  (new (*parser <XX>)
       (*parser <XX>)
       (*caten 2)
       (*pack-with
  (lambda (h l)
    (+ l (* 256 h))))
       done))

(define <hex-char>
  (new (*parser (word-ci "x"))

       (*parser <XXXX>)
       (*parser <XX>)
       (*disj 2)
       (*pack integer->char)

       (*caten 2)
       (*pack-with (lambda (x ch) ch))
       done))

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

(define <HexChar>
  (new 
       (*parser (range #\0 #\9))
       (*parser (range #\a #\f))

       (*disj 2)
done))

(define <HexUnicodeChar>
  (new 
       (*parser (char #\x))
       (*parser <HexChar>)
       (*parser <HexChar>) *star
       (*caten 3)

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

          (list->string `(#\ ,@b))))
      
done))

;; --------------------------------
;;           Number:
;; --------------------------------

(define <digit-0-9>
  (range #\0 #\9))

(define <digit-1-9>
  (range #\1 #\9))

(define <Natural>
  (new (*parser (char #\0))
       (*pack (lambda (_) 0))

       (*parser <digit-1-9>)
       (*parser <digit-0-9>) *star
       (*caten 2)
       (*pack-with
  (lambda (a s)
    (string->number
     (list->string
      `(,a ,@s)))))

       (*disj 2)
       done))

(define <Integer>
  (new (*parser (char #\+))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
  (lambda (++ n) n))

       (*parser (char #\-))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
  (lambda (-- n) (- n)))

       (*parser <Natural>)

       (*disj 3)

       done))

(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with
  (lambda (num div den)
    (/ num den)))
       done))

(define <Number>
  (new (*parser <Integer>)
       (*parser <Fractiodn>)
       (*disj 2)
       done))

;; --------------------------------
;;           Number:
;; --------------------------------



(define <StringVisibleChar>
  (new (*parser (range #\  #\~))
    ;; fix to be a string from space up.
       done))

(define <StringMetaChar>
  (new (*parser (range #\  #\~))
    ;; fix to be a meta string char.
       done))

(define <StringHexChar>
  (new (*parser (range #\  #\~))
    ;; fix to be a hex string char.
       done))

(test-string <StringVisibleChar> " espace")