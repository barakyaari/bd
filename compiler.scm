(load "pc.scm")


(define <EmptyParser>
  (new (*parser (range 
                  (integer->char 1)
                  
                  (integer->char 32)))
       *star
       done))

;; --------------------------------
;;           Comments:
;; --------------------------------

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
   (new (*parser (char #\newline))
        (*parser <end-of-input>)
        (*disj 2)
        done)))
    (new (*parser (char #\;))
   
   (*parser <any-char>)
   (*parser <end-of-line-comment>)
   *diff *star

   (*parser <end-of-line-comment>)
   (*caten 3)
   done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       done))

(define <comment>
  (disj <line-comment>
  <sexpr-comment>))

(define <skip>
  (disj <comment>
  <whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
     (*parser <p>)
     (*parser <wrapper>)
     (*caten 3)
     (*pack-with
      (lambda (_left e _right) e))
     done))))

(define ^<CommentOutPrefix*> (^^<wrapped> (star <skip>)))


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

(define <EmptyHexChar>
  (new 
    (*parser (char-ci #\x))
    (*parser <HexChar>)
    *not-followed-by
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
    (*parser <EmptyHexChar>)
    (*parser <HexUnicodeChar>)
    (*parser <VisibleSimpleChar>)
    (*parser <VisibleSimpleChar>)
    *not-followed-by
    (*disj 4)
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

(define <Zero>
  (new
    (*parser (char #\0))
    (*pack (lambda (_) 0))
    
    done))

(define <Natural>
  (new 
    (*parser (char #\0)) *star
    (*pack (lambda (_) 0))
    (*parser <digit-1-9>)
    (*parser <digit-0-9>) *star
    (*caten 2)
    (*pack-with
      (lambda (a s)
        (string->number
          (list->string
            `(,a ,@s)))))
    
    
    (*caten 2)
    (*pack-with
      (lambda (leadingzeros number)
        number))
    done))

(define <Integer>
  (new 
       (*parser (char #\-))
       (*parser <Natural>)
       (*parser <Zero>)
       (*disj 2)
       (*caten 2)
       (*pack-with

         (lambda (minus n) 
           (- n)))

    (*parser (char #\+))
       (*parser <EmptyParser>)
       (*parser <Natural>)
       (*parser <Zero>)
       (*disj 2)
       (*caten 3)
       (*pack-with
         (lambda (plus space n)
          n))
    
       (*parser <Natural>)
       (*parser <Zero>)
       
       (*disj 4)
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
    (*delayed (lambda () <SymbolChar>))
    *not-followed-by 
    done))

;; --------------------------------
;;           String:
;; --------------------------------


(define <StringLiteralChar>
  (new 
    (*parser (range (integer->char (string->number "1"))  (integer->char (string->number "1114111"))))
    (*parser (char #\\))  
    *diff  
       done))

(define <StringMetaChar>
  (new 
    (*parser (word "\\\\"))
    (*pack (lambda(_) #\\))
    (*parser (word "\\\""))
    (*pack (lambda(_) #\"))
    (*parser (word "\\t"))
    (*pack (lambda(_) #\tab))
    (*parser (word "\\f"))
    (*pack (lambda(_) #\page))
    (*parser (word "\\n"))
    (*pack (lambda(_) #\newline))
    (*parser (word "\\r"))
    (*pack (lambda(_) #\return))
    (*disj 6)
    done))

(define <StringHexChar>
  (new 
    (*parser (char #\\))
    (*parser (char-ci #\x))
    (*parser <HexChar>) *star
    (*parser (char #\;))

    (*caten 4)
    (*pack-with (lambda (slash x charlist semicolun)
        (display "StringHexChar: ")
        (display (integer->char
                    (string->number 
                      (list->string charlist) 16)))
        (display "\n")

                (integer->char
                    (string->number 
                      (list->string charlist) 16))))
    
    done))


(define <StringChar>
  (new 
    
    (*parser <StringMetaChar>)
    (*parser <StringLiteralChar>)
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
    (*pack (lambda (capital)
        (integer->char  (+ (char->integer capital) 32))))
    (*parser (char #\!))
    (*parser (char #\$))
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
    (*disj 15)
    
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
         (list 'quote  e)))
    done))

(define <QuasiQuoted>
  (new 
    (*parser (char #\`))
    (*delayed (lambda () <sexpr>))
    (*caten 2)
    (*pack-with
      (lambda(sign e)

         (list 'quasiquote  e)))
    done))

(define <Unquoted>
  (new 
    (*parser (char #\,))
    (*delayed (lambda () <sexpr>))
    (*caten 2)
    (*pack-with
      (lambda(sign e)
        (list 'unquote  e)))
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

(define <InfixCommentExpression>
  (new 
        (*parser (word "#;"))
        (*parser <EmptyParser>)
       (*delayed (lambda () <InfixExpression>))
       (*parser <EmptyParser>)
       (*caten 4)
       done))

(define <InfixComment>
  (new 
    (*parser <line-comment>)
    (*parser <InfixCommentExpression>)
    (*disj 2)

    done))


(define <InfixNumber>
  (new 
    (*parser <Fraction>)
    (*parser <Integer>)
    (*disj 2)
    (*delayed (lambda () <InfixSymbolChar>))
    *not-followed-by 

    done))

(define <InfixSymbolChar>
  (new 
    
    (*parser (range #\0 #\9))
    (*parser (range #\a #\z))
    (*parser (range #\A #\Z))
    (*pack (lambda (capital)
        (integer->char  (+ (char->integer capital) 32))))
    (*parser (char #\!))
    (*parser (char #\$))
    (*parser (char #\_))
    (*parser (char #\=))
    (*parser (char #\<))
    (*parser (char #\>))
    (*parser (char #\?))
    (*disj 10)
    done))

(define <InfixSymbol>
  (new
    (*parser <InfixNumber>) 
    (*parser <InfixSymbolChar>) *plus
    *not-followed-by
    
    (*parser <digit-0-9>) *star
    (*parser <InfixSymbolChar>) *plus
    (*parser <digit-0-9>) *star
    (*caten 3)
        (*pack-with 
          (lambda (numbers1 chars numbers2)
            `(,@numbers1
              ,@chars
               ,@numbers2)))
        (*pack 
          (lambda (_)
            (string->symbol
                    (list->string _))))        
    (*disj 2)

    done))

(define <InfixFinal>
  (new
    (*parser <EmptyParser>) 
    (*parser <InfixSymbol>)
    (*parser <InfixNumber>)
    (*delayed (lambda () <InfixSexprEscape>))

    (*disj 3)
    
    (*parser <EmptyParser>)
    
    (*caten 3)
    (*pack-with (lambda (space1 expression space2)
      expression))
    done))

(define <InfixParen>
    (new
    (*parser <EmptyParser>)
    (*parser (char #\())
    (*parser <EmptyParser>)
    (*delayed (lambda () <InfixAddOrSub>))
    (*parser <EmptyParser>)
    (*parser (char #\)))
    (*parser <EmptyParser>)
    (*caten 7)
    
    (*pack-with
        (lambda (space open space1 expression space2 close space3)
           expression))
    done))

(define <PowerSymbol>
    (new
    (*parser (word "**"))
    (*parser (char #\^))
    (*disj 2)
    done))

(define <InfixArrayGet>
  
    (new
    (*parser <EmptyParser>)
    (*parser <InfixFinal>)
    (*parser (char #\())
     *not-followed-by       
    (*parser <InfixParen>)
    (*disj 2)
    
    (*parser <EmptyParser>)
    (*parser (char #\[))
          
    (*parser <EmptyParser>)
    (*delayed (lambda () <InfixExpression>))
    (*parser <EmptyParser>)
    (*parser (char #\]))
    (*parser <EmptyParser>)
    (*caten 7)
    (*pack-with
      (lambda (space1 open space2 Expression space3 close space4)
        Expression
        ))
    *plus
    (*caten 2)
       (*pack-with 
         (lambda (array lista1)
            (if (equal? 1 (length lista1))
                 `(vector-ref ,array ,(car lista1))
              (if (equal? 0 (length lista1))
                  array
                    (letrec 
                      ((loopPrint
                          (lambda (expression lista2)
                            (if (equal? 1 (length lista2)) 
                                `(vector-ref ,expression ,(car lista2))
                                (loopPrint
                                  `(vector-ref ,expression ,(car lista2)) (cdr lista2))
                              ))))
                      
                    (loopPrint `(vector-ref ,array ,(car lista1)) (cdr lista1)))))))
    (*parser <EmptyParser>)
    (*caten 3)
    (*pack-with (lambda (space expression space2)
                  expression))
    done))
        

(define <InfixPow>
    (new
    (*parser <EmptyParser>)
    
    (*parser <InfixArrayGet>)
    (*parser <InfixParen>)
    (*delayed (lambda () <InfixFuncall>))
    (*disj 3)
    (*parser <EmptyParser>)
    (*parser <PowerSymbol>)
    (*parser <EmptyParser>)
    (*parser <InfixArrayGet>)
    (*parser <InfixParen>)
    (*delayed (lambda () <InfixFuncall>))
    (*disj 3)
    
    (*parser <EmptyParser>)

    (*caten 5) ;(Power + number remain)
    (*pack-with
        (lambda (space pow space2 num2 space3)
           num2))
    *star ;( (power+rest)*)
    (*caten 2) ;(number (power+rest)*)
    
    (*pack-with (lambda (num2 lista)
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
    (*parser <EmptyParser>)
    (*caten 3)
    (*pack-with 
      (lambda (space1 expression space2)
        expression))
    done))

(define <InfixMulOrDiv>
    (new
    (*parser <EmptyParser>)

    (*parser <InfixPow>)
    (*parser <EmptyParser>)
    (*parser (word "*"))
    (*parser (word "/"))
    (*disj 2)
    (*parser <EmptyParser>)
    (*parser <InfixPow>)
    (*parser <EmptyParser>)

    (*caten 5) ;(Sign + number remain)
    (*pack-with
        (lambda (space mulOrDiv space2 num2 space3)
            `(,(string->symbol
                  (list->string mulOrDiv)) ,num2)))
    *star ;( (Sign+number)*)
    (*caten 2) ;(number (Sign+number)*)
    
    (*pack-with (lambda (num2 lista)
                  (letrec 
                    ((loopPrint
                      (lambda (num1 lista1)
                        (if (equal? (length lista1) 0) num1
                        (if (equal? (length lista1) 1) `(,(caar lista1) ,num1 ,(cadar lista1))
                            ;Longer that 1:
                            (loopPrint `(,(caar lista1) ,num1 ,(cadar lista1)) (cdr lista1)))))))
                              (loopPrint num2 lista)
                            )))
    (*parser <EmptyParser>)
    (*caten 3)
    (*pack-with (lambda (space1 expression space2)
            expression))
    done))

(define <InfixAddOrSub>
    (new
    (*parser <InfixComment>) *star
    (*parser <EmptyParser>)
    (*disj 2)
    (*parser <InfixMulOrDiv>)
        (*parser (word "-"))
           (*parser <InfixMulOrDiv>)
            (*caten 2)
         (*pack-with (lambda (minus expression)
            `(- ,expression)))
        (*disj 2)
        
    (*parser <EmptyParser>)
    (*parser <InfixComment>) *star
    (*parser (word "-"))
    (*parser (word "+"))
    (*disj 2)

    (*parser <EmptyParser>)
    (*parser <InfixComment>) *star
    (*parser <InfixMulOrDiv>)
        (*parser (word "-"))
           (*parser <InfixMulOrDiv>)
            (*caten 2)
         (*pack-with (lambda (minus expression)
            `(- ,expression)))
        (*disj 2)
    (*parser <EmptyParser>)
    (*parser <InfixComment>) *star
    (*caten 8)
    (*pack-with
        (lambda (space1 comment1 addOrSub space2 comment2 num2 space3 comment3)
           `(,(string->symbol (list->string addOrSub))
             ,num2)))
    *star
    (*caten 2)
    
    (*pack-with (lambda (num2 lista)
                  (letrec 
                    ((loopPrint
                      (lambda (num1 lista1)
                        (if (equal? (length lista1) 0) num1
                        (if (equal? (length lista1) 1) `(,(caar lista1) ,num1 ,(cadar lista1))
                            ;Longer that 1:
                            (loopPrint `(,(caar lista1) ,num1 ,(cadar lista1)) (cdr lista1)))))))
                              (loopPrint num2 lista)
                            )))
    (*parser <EmptyParser>)
    (*caten 3)

    (*pack-with 
      (lambda (space1 expression space2)
          expression))
    done))

(define <InfixArgList>
  
  (new
    (*parser <EmptyParser>)
    (*parser <InfixComment>) *star

 (*delayed (lambda () <InfixExpression>)) 
   
    (*parser <EmptyParser>)
    (*parser <InfixComment>) *star

    (*parser (char #\,))
    (*parser <EmptyParser>)
 (*delayed (lambda () <InfixExpression>))    

    (*parser <EmptyParser>)
    (*parser <InfixComment>) *star

    (*caten 7)
    (*pack-with 
      (lambda (space1 comment0 psik space2 addOrSub comment1 space3)
                  addOrSub
                  ))
    *star
    (*parser <EmptyParser>)
    (*caten 5)
    (*pack-with 
      (lambda (space1 comment1 addOrSub1 list space2)
        `(,addOrSub1 ,@list)))
    (*parser <epsilon>) ;In Meir's parsers
    (*disj 2)
    done))
    
(define <InfixFuncall>
  (new
     
    (*parser <EmptyParser>)
    (*parser <InfixFinal>)

    (*parser <EmptyParser>)
    (*parser (char #\())
    (*parser <EmptyParser>)
    (*parser <InfixComment>) *star
    (*parser <InfixArgList>)
    (*parser <EmptyParser>)
    (*parser <InfixComment>) *star
    (*parser (char #\)))
    (*parser <EmptyParser>)
    (*caten 9)
    (*pack-with 
      (lambda (space open space1 comment1 arglist space2 comment2 close space3)
                  arglist))
    *star
    (*caten 2)
      (*pack-with 
            (lambda (function expression)
                 ; only final:
                (if (equal? 0 (length expression))
                    function
                    ; Length of 1:
                    (if (equal? 1 (length expression))
                        `(,function ,@(car expression))
                        
                     (letrec ((loopPrint
                      (lambda (expression1 lst1)
                         (if (equal? (length lst1) 1) `(,expression1 ,@(car lst1))
                             (loopPrint `(,expression1 ,@(car lst1)) (cdr lst1))))))
                             (loopPrint `(,function ,@(car expression)) (cdr expression)))))))
    
    (*parser <EmptyParser>)
    (*caten 3)
    (*pack-with 
      (lambda (space1 functionOrExpression space2)
                  functionOrExpression))
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

(define <InfixSexprEscape>
    (new
    (*parser <EmptyParser>)
    (*parser <InfixPrefixExtensionPrefix>)
    (*parser <EmptyParser>)
    (*delayed (lambda () <sexpr>))
    (*parser <EmptyParser>)
    (*caten 5)
    (*pack-with 
      (lambda (space pre space2 sexpr space3)
          sexpr))
    done))

(define <InfixExpression>

    (new
      (*parser <EmptyParser>)
      (*parser <InfixAddOrSub>)
      
      
      (*parser <EmptyParser>)

      (*caten 3)
      (*pack-with (lambda (space expression space2)
                    expression ))
      done))

(define <InfixExtension>
  (new 
    (*parser <InfixComment>) *star

    (*parser <InfixPrefixExtensionPrefix>) 
    (*parser <InfixExpression>)
    (*caten 3) 
    (*pack-with (lambda (comment1 prefix expre)
                  expre))
    done))

; ====================================================
; ====================================================


(define <sexpr1> <sexpr>)

(define <sexpr>
  (^<CommentOutPrefix*>
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
    done)))



; ====================================================
; ====================================================