(load "pc.scm")

(define <EmptyParser>
  (new (*parser (range 
                  (integer->char 1)
                  
                  (integer->char 32)))
       *star
       done))


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
    (*parser <HexChar>) *plus
    (*caten 2)
    (*pack-with (lambda(x lista)
                  (integer->char
                    (string->number 
                      (list->string lista) 16))))
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
    (*pack(lambda(_)
            (display "Number: ")
            (display _)
            (display "\n")
             _))
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
    (*pack-with (lambda (slash x charlist)
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
    (*parser <SymbolChar>) *plus
    (*pack (lambda(_)
                  (string->symbol
                    (list->string _))))
    done))

;; --------------------------------
;;           Lists:
;; --------------------------------

(define <ProperList>
  (new 
    (*parser (char #\())
    (*parser <EmptyParser>)
    (*delayed (lambda () <sexpr>))
    *star
    (*parser (char #\)))
    (*caten 4)
    (*pack-with
      (lambda(open emptyparser expr1 close)
        (display 'properList)
        `(,@expr1 )))
    done))

(define <ImproperList>
  (new 
        (*parser (char #\())
        (*delayed (lambda () <sexpr>))
         *plus

        (*parser (char #\.))
        (*delayed (lambda () <sexpr>))
        (*parser (char #\)))
        (*caten 5)
        (*pack-with
          (lambda(open expr1 point expr2 close)
            (display 'improperList)
          `(,@expr1  . ,expr2 )))

       done))



(define <Vector>
  (new 
    (*parser (char #\#))
    (*parser (char #\())
    (*delayed (lambda () <sexpr>))
    *star
             
    (*parser (char #\)))
    (*caten 4)
    (*pack-with
      (lambda (a b lista d)
        (list->vector lista )))
    done))

(define <Quoted>
  (new 
    (*parser (char #\'))
    (*delayed (lambda () <sexpr>))
    (*caten 2)
    (*pack-with
      (lambda(sign e)
        `(,'quote ,@e)))
    done))

(define <QuasiQuoted>
  (new 
    (*parser (char #\`))
    (*delayed (lambda () <sexpr>))
    (*caten 2)
    (*pack-with
      (lambda(sign e)
        `(,'quasiquote ,@e)))
    done))

(define <Unquoted>
  (new 
    (*parser (char #\,))
    (*delayed (lambda () <sexpr>))
    (*caten 2)
    (*pack-with
      (lambda(sign e)
        `(,'unquote ,@e)))
    done))

(define <UnquoteAndSpliced>
  (new 
    (*parser (char #\,))
    (*parser (char #\@))
    (*delayed (lambda () <sexpr>))
    (*caten 3)
    (*pack-with
      (lambda(sign strudel e)
        (list 'unquote-splicing e)))
    done))

;; --------------------------------
;;           Infix:
;; --------------------------------
(define <InfixNeg>
    (new
    (*parser <EmptyParser>)
    (*parser (char #\-))
    (*parser <EmptyParser>)
    (*delayed (lambda () <InfixExpression>))
    (*caten 4)
    
    (*pack-with
        (lambda (space minus space expression)
           `(- ,expression)))
    done))

(define <InfixSub>
    (new
    (*parser <Number>)
    
    (*parser <EmptyParser>)
    (*parser (char #\-))
    (*parser <EmptyParser>)
    (*parser <Number>)
    (*parser <EmptyParser>)

    (*caten 5) ;(Power + number remain)
    (*pack-with
        (lambda (space add space2 num2 space3)
           num2))
    *star ;( (power+number)*)
    (*caten 2) ;(number (power+number)*)
    
    (*pack-with (lambda (num2 lista)
        (display "Add\n")
                  (letrec 
                    ((loopPrint
                      (lambda (num1 lista1)
                        (if (equal? (length lista1) 0) num1
                        (if (equal? (length lista1) 1) `(- ,num1 ,@lista1)
                            ;Longer that 1:
                            (loopPrint `(- ,num1 ,(car lista1)) (cdr lista1)))))))
                              (loopPrint num2 lista)
                            )))
    done))


(define <InfixAdd>
    (new
    (*parser <Number>)
    
    (*parser <EmptyParser>)
    (*parser (char #\+))
    (*parser <EmptyParser>)
    (*parser <Number>)
    (*parser <EmptyParser>)

    (*caten 5) ;(Power + number remain)
    (*pack-with
        (lambda (space add space2 num2 space3)
           num2))
    *star ;( (power+number)*)
    (*caten 2) ;(number (power+number)*)
    
    (*pack-with (lambda (num2 lista)
        (display "Add\n")
                  (letrec 
                    ((loopPrint
                      (lambda (num1 lista1)
                        (if (equal? (length lista1) 0) num1
                        (if (equal? (length lista1) 1) `(+ ,num1 ,@lista1)
                            ;Longer that 1:
                            (loopPrint `(+ ,num1 ,(car lista1)) (cdr lista1)))))))
                              (loopPrint num2 lista)
                            )))
    done))

(define <PowerSymbol>
    (new
    (*parser (word "**"))
    (*parser (char #\^))
    (*disj 2)
    done))

(define <InfixPow>
    (new
    (*parser <Number>)
    
    (*parser <EmptyParser>)
    (*parser <PowerSymbol>)
    (*parser <EmptyParser>)
    (*parser <Number>)
    (*parser <EmptyParser>)

    (*caten 5) ;(Power + number remain)
    (*pack-with
        (lambda (space pow space2 num2 space3)
           num2))
    *star ;( (power+number)*)
    (*caten 2) ;(number (power+number)*)
    
    (*pack-with (lambda (num2 lista)
        (display "power\n")
                  (letrec 
                    ((loopPrint
                      (lambda (num1 lista1)
                        (if (equal? (length lista1) 0) num1
                        (if (equal? (length lista1) 1) `(expt ,num1 ,@lista1)
                        (if (equal? (length lista1) 2)
                            `(expt ,num1 (expt ,(car lista1) ,(cadr lista1)))
                            ;Longer that 2:
                            `(expt ,num1 ,(loopPrint (car lista1) (cdr lista1)))))))))
                              (loopPrint num2 lista)
                            )))
    done))

(define <InfixExpression>
    (new
    (*parser <InfixNeg>)
    done))

(define <InfixPrefixExtensionPrefix>
  (new 
    (*parser <EmptyParser>)
    (*parser (word "##"))
    (*parser (word "#%"))
    (*disj 2)
    (*parser <EmptyParser>)
    (*caten 3)
    done))

(define <InfixExtension>
  (new 
    (*parser <InfixPrefixExtensionPrefix>) 
    (*parser <InfixExpression>)
    (*caten 2) 
    (*pack-with (lambda (prefix expre)
    (display "Infix Extention\n")
                  expre))
    done))

; ====================================================
; ====================================================

(define <sexpr>
  (new 
    (*parser <EmptyParser>)
    (*parser <Boolean>)
    (*parser <Char>)
    (*parser <Number>)
    (*parser <String>)
    (*parser <Symbol>)
    (*parser <ProperList>)
    (*parser <ImproperList>)
    (*parser <Vector>)
    (*parser <Quoted>)
    (*parser <QuasiQuoted>)
    (*parser <UnquoteAndSpliced>)
    (*parser <Unquoted>)
    (*parser <InfixExtension>)
    (*disj 13)     
    (*parser <EmptyParser>)
    (*caten 3)
    (*pack-with
      (lambda (space expr space2)
        expr))
    done))
; ====================================================
; ====================================================