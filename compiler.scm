(load "pc.scm")

;; --------------------------------
;;           Boolean:
;; --------------------------------
(define <Boolean>
  (new 
	;; True:
  	(*parser (char #\#))
       (*parser (char-ci #\t))

       (*caten 2)
       (*pack-with
       	(lambda (a b)
	  #t))
	;; False:
  	   (*parser (char #\#))
       (*parser (char-ci #\f))

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
    (*pack (lambda (_) (integer->char 955)))

    (*parser (word "newline"))
    (*pack (lambda (_) (integer->char 10)))

    (*parser (word "nul"))
    (*pack (lambda (_) (integer->char 0)))

    (*parser (word "page"))
    (*pack (lambda (_) (integer->char 12)))

    (*parser (word "return"))
    (*pack (lambda (_) (integer->char 13)))

    (*parser (word "space"))
    (*pack (lambda (_) (integer->char 32)))

    (*parser (word "tab"))
    (*pack (lambda (_) (integer->char 9)))

       (*disj 7)
       done))

(define <HexChar>
  (new
      (*parser (range #\0 #\9))
      (*parser (range-ci #\a #\f))
      (*disj 2)
       done))

(define <HexUnicodeChar>
  (new 
       (*parser (char-ci #\x))
       (*parser <HexChar>)
       (*parser <HexChar>) *star
       (*caten 3)
       (*pack-with (lambda(x first rest)
        (integer->char
          (string->number 
              (list->string `(,first ,@rest)) 16))))
done))

(define <Char>
  (new 
       (*parser <CharPrefix>)
       (*parser <NamedChar>)
       (*parser <HexUnicodeChar>)
       (*parser <VisibleSimpleChar>)
       (*disj 3)
       (*caten 2)
       (*pack-with
        (lambda(a b)
          b))
      
done))

;; --------------------------------
;;           Number:
;; --------------------------------

(define <digit-0-9>
  (range #\0 #\9))

(define <digit-1-9>
  (range #\1 #\9))

(define <Natural>
  (new 
  (*parser (char #\0)) *star

  (*parser (char #\0))
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
      (*caten 2)
      (*pack-with
         (lambda (leadingzeros number)
            number))
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
  (new 
       (*parser <Fraction>)
       (*parser <Integer>)
       
       (*disj 2)
       done))

;; --------------------------------
;;           String:
;; --------------------------------


(define <StringVisibleChar>
  (new (*parser (range #\!  (integer->char (string->number "1114111"))))
       done))

(define <StringMetaChar>
  (new 
    (*parser (word "\\\\"))
       (*pack (lambda(_) #\\))
    (*parser (word "\\\""))
       (*pack (lambda(_) #\"))
    (*parser (word "\t"))
       (*pack (lambda(_) #\tab))
    (*parser (word "\f"))
       (*pack (lambda(_) #\page))
    (*parser (word "\\n"))
       (*pack (lambda(_) #\newline))
    (*parser (word "\r"))
       (*pack (lambda(_) #\return))
       (*disj 6)

       done))

(define <StringHexChar>
  (new 
        (*parser (char #\\))
    (*parser (char-ci #\x))
    (*parser <HexChar>) *star
    (*caten 3)
    (*pack-with (lambda(slash x charlist)
        (integer->char
          (string->number 
              (list->string charlist) 16))))

       done))

    
(define <StringChar>
  (new 

        (*parser <StringMetaChar>)
        (*parser <StringVisibleChar>)
        (*parser <StringHexChar>)

        (*disj 3)
       done))


(define <String>
  (new 
    (*parser (word "\""))

    (*parser <StringChar>)
    (*parser (word "\""))
       *diff
        *star
    (*parser (word "\""))


(*caten 3)

    (*pack-with
    (lambda(intro word outro)
        (list->string word)))
       done))

;; --------------------------------
;;           Symbol:
;; --------------------------------

(define <SymbolChar>
  (new 

        (*parser (range #\0 #\9))
        (*parser (range #\a #\z))
        (*parser (range #\A #\Z))
        (*parser (char #\^))
        (*parser (char #\*))
        (*parser (char #\-))
        (*parser (char #\_))
        (*parser (char #\=))
        (*parser (char #\+))
        (*parser (char #\<))
        (*parser (char #\>))
        (*parser (char #\?))
        (*parser (char #\/))
        (*disj 13)

       done))

(define <Symbol>
  (new 
        (*parser <SymbolChar>)
        (*parser <SymbolChar>) *star
        (*caten 2)
        (*pack-with (lambda(first rest)
                      (string->symbol
                        (list->string `(,first ,@rest)))))

       done))


;; --------------------------------
;;           Infix:
;; --------------------------------


(define <InfixPrefixExtensionPrefix>
  (new 
        (*parser (word "##"))
        (*parser (word "#%"))
        (*disj 2)
       done))


(define <InfixExtension>
  (new 
        (*parser <InfixPrefixExtensionPrefix)
        (*parser <InfixExpression>)
        (*disj 2)
       done))


(define <InfixExtension>
  (new 
        (*parser <InfixPrefixExtensionPrefix)
        (*parser <InfixExpression>)
        (*disj 2)
       done))