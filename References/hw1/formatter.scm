(load "pc.scm")

;; parsers from the package tutorial
(define ^<meta-char>
	(lambda (str ch)
		(new (*parser (word str))
			(*pack (lambda (_) ch))
			done)))
			
(define <string-meta-char>
	(new (*parser (^<meta-char> "\\\\" #\\))
		(*parser (^<meta-char> "\\\"" #\"))
		(*parser (^<meta-char> "\\n" #\newline))
		(*parser (^<meta-char> "\\r" #\return))
		(*parser (^<meta-char> "\\t" #\tab))
		(*parser (^<meta-char> "\\f" #\page)) ; formfeed
		(*parser (^<meta-char> "\\{lambda}" (integer->char 955)))
		(*parser (^<meta-char> "\\{alef}" (integer->char 1488)))
		(*parser (^<meta-char> "\\{bismillah}" (integer->char 65021)))
		(*parser (^<meta-char> "\\{smiley}" (integer->char 9786)))
		(*parser (^<meta-char> "\\{newline}" #\newline))
		(*parser (^<meta-char> "\\{return}" #\return))
		(*parser (^<meta-char> "\\{tab}" #\tab))
		(*parser (^<meta-char> "\\{page}" #\page))
		(*parser (^<meta-char> "\\{" #\{))
		(*parser (^<meta-char> "\\}" #\}))
		(*parser (^<meta-char> "~~" #\~))
		(*disj 17)
		done))

(define <string-char>
	(new (*parser <string-meta-char>)
		(*parser <any-char>)
		(*parser (char #\"))
		(*parser (char #\\))
		(*parser (char #\~))
		(*parser (char #\{))
		(*parser (char #\}))
		(*disj 5)
		*diff
		(*disj 2)
		done))
 
(define <digit-0-9>
	(range #\0 #\9))
 
(define <digit-1-9>
	(range #\1 #\9))
 
(define <nat>
	(new (*parser (char #\0))
		(*pack (lambda (_) 0))
		(*parser <digit-1-9>)
		(*parser <digit-0-9>) *star
		(*caten 2)
		(*pack-with
			(lambda (a s)
				(string->number (list->string `(,a ,@s)))))
		(*disj 2)
		done))

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
	(new (*parser <XXXX>)
		(*parser <XX>)
		(*disj 2)
		(*pack integer->char)
		done))

(define <hex-symbol>
	(new (*parser (char-ci #\u))
		(*parser (char-ci #\h))
		(*parser (char-ci #\x))
		(*parser (char #\1))
		(*parser (char #\6))
		(*parser (char #\#))
		(*caten 3)
		(*disj 4)
		done))
 
(define <hex>
	(new (*parser (word "\\"))
		(*parser <hex-symbol>)
		(*parser <hex-char>)
		(*caten 3)
		(*pack-with 
			(lambda (slashes sym ch)
				ch))
		done))

(define <str-hex>
	(new (*parser <string-char>) *star
		(*parser <hex>)
		(*parser <string-char>) *star
		(*caten 3)
		(*pack-with 
			(lambda (chars1 hex chars3)
				(list->string (append chars1 (list hex) chars3))))
		done))
		
(define <str-with-hex>
	(new (*parser <str-hex>) *plus
		(*pack-with 
			(lambda (str . lst)
				(string-append str (apply string-append lst))))
		done))
		
(define <st>
	(new (*parser <str-with-hex>)
		(*parser <string-char>) *star
		(*pack
			(lambda (chars)
				(list->string chars)))
		(*disj 2)
	done))

(define <valid-char>
	(new (*parser <string-meta-char>)
		(*parser <any-char>)
		(*parser (char #\"))
		(*parser (char #\\))
		(*disj 2)
		*diff
		(*disj 2)
		done))
	
(define <valid-string>
	(new (*parser <str-with-hex>)
		(*parser <valid-char>) *star
		(*pack
			(lambda (chars)
				(list->string chars)))
		(*disj 2)
	done))
		
(define <whitespace>
	(const
		(lambda (ch)
			(char<=? ch #\space))))

(define delete-spaces
	(lambda (chars)
		(if (null? chars) 
			chars
			(<whitespace> chars
				(lambda (match remaining)
					(delete-spaces remaining))
				(lambda (_)
					(cons (car chars) (delete-spaces (cdr chars))))))))
			
(define <template>
	(new (*parser (word "~{"))
		(*parser <st>)
		(*parser (char #\}))
		(*caten 3)
		(*pack-with
			(lambda (open chars close)
				(list->string (delete-spaces (string->list chars)))))
		done))

(define <comment>
	(new (*parser (word "~{{"))
		(*parser <any-char>)
		(*parser (word "}}"))
		*diff
		(*parser (word "~{{"))
		*diff
		(*delayed (lambda () <comment>))
		(*parser (word "}}"))
		*diff
		*star
		(*caten 2)
		*star
		(*parser (word "}}"))
		(*caten 3)
		(*pack-with
			(lambda (open chars close)
				(lambda (_) '())))
		done))
		
(define <st-or-comment>
	(new (*parser <comment>)
		(*parser <st>)
		(*disj 2)
		done))
		
(define <fixed-arrow-right>
	(new (*parser (char #\~))
		(*parser (char #\-)) *plus
		(*caten 2)
		(*parser <nat>)
		(*parser (char #\-)) *plus
		(*parser (char #\>))
		(*parser (char #\{))
		(*caten 3)
		(*parser <st>)
		(*parser (char #\}))
		(*caten 5)
		(*pack-with
			(lambda (open num arrow templ close)
				(cons num (list->string (delete-spaces (string->list templ))))))
		done))
		
(define <fixed-arrow-left>
	(new (*parser (char #\~))
		(*parser (char #\<))
		(*parser (char #\-)) *plus
		(*caten 3)
		(*parser <nat>)
		(*parser (char #\-)) *plus
		(*parser (char #\{))
		(*caten 2)
		(*parser <st>)
		(*parser (char #\}))
		(*caten 5)
		(*pack-with
			(lambda (open num arrow templ close)
				(cons num (list->string (delete-spaces (string->list templ))))))
		done))
		
(define <var-arrow-right>
	(new (*parser (char #\~))
		(*parser (char #\-)) *plus
		(*parser (char #\{))
		(*caten 3)
		(*parser <st>)
		(*parser (char #\}))
		(*parser (char #\-)) *plus
		(*parser (char #\>))
		(*parser (char #\{))
		(*caten 4)
		(*parser <st>)
		(*parser (char #\}))
		(*caten 5)
		(*pack-with
			(lambda (open width arrow templ close)
				(cons width (list->string (delete-spaces (string->list templ))))))
		done))
		
(define <var-arrow-left>
	(new (*parser (char #\~))
		(*parser (char #\<))
		(*parser (char #\-)) *plus
		(*parser (char #\{))
		(*caten 4)
		(*parser <st>)
		(*parser (char #\}))
		(*parser (char #\-)) *plus
		(*parser (char #\{))
		(*caten 3)
		(*parser <st>)
		(*parser (char #\}))
		(*caten 5)
		(*pack-with
			(lambda (open width arrow templ close)
				(cons width (list->string (delete-spaces (string->list templ))))))
		done))

(define <var-arrow-center>
	(new (*parser (char #\~))
		(*parser (char #\<))
		(*parser (char #\-)) *plus
		(*parser (char #\{))
		(*caten 4)
		(*parser <st>)
		(*parser (char #\}))
		(*parser (char #\-)) *plus
		(*parser (char #\>))
		(*parser (char #\{))
		(*caten 4)
		(*parser <st>)
		(*parser (char #\}))
		(*caten 5)
		(*pack-with
			(lambda (open width arrow templ close)
				(cons width (list->string (delete-spaces (string->list templ))))))
		done))

(define <is-template>
	(new (*parser <template>)
		(*parser <fixed-arrow-right>)
		(*parser <fixed-arrow-left>)
		(*parser <var-arrow-right>)
		(*parser <var-arrow-left>)
		(*parser <var-arrow-center>)
		(*disj 6)
		done))

 ;------------------------------------------------------------------------------------------------------;
		
(define get-match
	(lambda (alist)
		(cadr (assoc 'match alist))))
		
(define get-remaining
	(lambda (alist)
		(cdr (assoc 'remaining alist))))
				
(define get-next
	(lambda (p input)
		(p (string->list input)
			(lambda (match remaining)
				(if (null? remaining)
					(cons match "")
					(cons match (list->string remaining))))
			(lambda (_)
				(cons "" "")))))
				
; if the template "key" has a value, add "~a" to the param-string and it's value to the end of param-list
; else return #f
(define add-template
	(lambda (param-string key param-list alist)
		(let ((key-value (assoc key alist)))
			(if (boolean? key-value)
				#f
				(let ((new-str (string-append param-string "~a"))
					  (new-lst (append param-list (cdr key-value))))
					(cons new-str new-lst))))))
		
(define align
	(lambda (param-string key param-list alist num direction)
		(let ((key-value (assoc key alist)))
			(if (boolean? key-value)
				#f
				(let ((new-str (string-append param-string "~a"))
					  (new-lst (append param-list (list (direction (cadr key-value) num)))))
					(cons new-str new-lst))))))

(define add-spaces-left
	(lambda (str num)
	   (let ((len (string-length str)))
			(cond ((= num len) str)
				  ((> num len) (string-append (make-string (- num len) #\space) str))
				  (else
					(string-append (string (integer->char 9756)) (substring str (+ (- len num) 1) len)))))))
		
(define add-spaces-right
	(lambda (str num)
	   (let ((len (string-length str)))
			(cond ((= num len) str)
				  ((> num len) (string-append str (make-string (- num len) #\space)))
				  (else
					(string-append (substring str 0 (- num 1)) (string (integer->char 9758))))))))

(define up
	(lambda (str num)
		(if (or (and (= num 1) (> (string-length str) 1)) (and (= num 2) (> (string-length str) 2)))
			(string (integer->char 9757))
			(let* ((len (string-length str))
				   (sub-right (substring str 0 (floor (/ len 2))))
				   (sub-left (substring str (floor (/ len 2)) len))
				   (half-num (/ num 2)))
				(string-append (add-spaces-left sub-right (floor  half-num))
							   (add-spaces-right sub-left (ceiling  half-num)))))))
		
		
; this function recursively creates the parameters for the function "format"
(define create-params
	(lambda (input-string alist param-string param-list)
		; search for the next string / comment / template
		(let ((next-string (car (get-next <st> input-string)))
			  (rest-string (cdr (get-next <st> input-string)))
			  (next-comment (car (get-next <comment> input-string)))
			  (rest-comment (cdr (get-next <comment> input-string)))
			  (next-template (car (get-next <template> input-string)))
			  (rest-template (cdr (get-next <template> input-string)))
			  (next-fixed-arrow-right (car (get-next <fixed-arrow-right> input-string)))
			  (rest-fixed-arrow-right (cdr (get-next <fixed-arrow-right> input-string)))
			  (next-fixed-arrow-left (car (get-next <fixed-arrow-left> input-string)))
			  (rest-fixed-arrow-left (cdr (get-next <fixed-arrow-left> input-string)))
			  (next-var-arrow-right (car (get-next <var-arrow-right> input-string)))
			  (rest-var-arrow-right (cdr (get-next <var-arrow-right> input-string)))
			  (next-var-arrow-left (car (get-next <var-arrow-left> input-string)))
			  (rest-var-arrow-left (cdr (get-next <var-arrow-left> input-string)))
			  (next-var-arrow-center (car (get-next <var-arrow-center> input-string)))
			  (rest-var-arrow-center (cdr (get-next <var-arrow-center> input-string))))
			(cond 
				; if the next token is a string, add it to the param-string and continue recursively
				((not (equal? next-string ""))
						(create-params rest-string alist (string-append param-string next-string) param-list))
				; if the next token is a comment, don't add it to the param-string and continue recursively
				((not (equal? next-comment ""))
						(create-params rest-comment alist param-string param-list))
				; if the next token is a template, add "~a" to the param-string and it's value to the end of param-list
				; and continue recursively
				((not (equal? next-template ""))
						(let ((new-template (add-template param-string next-template param-list alist)))
							(if (equal? new-template #f)
								#f
								(create-params rest-template alist (car new-template) (cdr new-template)))))
				((not (equal? next-fixed-arrow-right ""))
						(let* ((templ (cdr next-fixed-arrow-right))
							   (num (car next-fixed-arrow-right))
							   (new-params (align param-string templ param-list alist num add-spaces-left)))
							(if (equal? new-params #f)
								#f
								(create-params rest-fixed-arrow-right alist (car new-params) (cdr new-params)))))
				((not (equal? next-fixed-arrow-left ""))
						(let* ((templ (cdr next-fixed-arrow-left))
							   (num (car next-fixed-arrow-left))
							   (new-params (align param-string templ param-list alist num add-spaces-right)))
							(if (equal? new-params #f)
								#f
								(create-params rest-fixed-arrow-left alist (car new-params) (cdr new-params)))))
				((not (equal? next-var-arrow-right ""))
						(let* ((templ (cdr next-var-arrow-right))
							   (width (car next-var-arrow-right))
							   (width-pair (assoc width alist)))
							(if (equal? width-pair #f)
								#f
							   (let* ((num (string->number (cadr width-pair)))
									  (new-arrow (align param-string templ param-list alist num add-spaces-left)))
									(if (equal? new-arrow #f)
										#f
										(create-params rest-var-arrow-right alist (car new-arrow) (cdr new-arrow)))))))
				((not (equal? next-var-arrow-left ""))
						(let* ((templ (cdr next-var-arrow-left))
							   (width (car next-var-arrow-left))
							   (width-pair (assoc width alist)))
							(if (equal? width-pair #f)
								#f
							   (let* ((num (string->number (cadr width-pair)))
									  (new-arrow (align param-string templ param-list alist num add-spaces-right)))
									(if (equal? new-arrow #f)
										#f
										(create-params rest-var-arrow-left alist (car new-arrow) (cdr new-arrow)))))))
				((not (equal? next-var-arrow-center ""))
						(let* ((templ (cdr next-var-arrow-center))
							   (width (car next-var-arrow-center))
							   (width-pair (assoc width alist)))
							(if (equal? width-pair #f)
								#f
							   (let* ((num (string->number (cadr width-pair)))
									  (new-arrow (align param-string templ param-list alist num up)))
									(if (equal? new-arrow #f)
										#f
										(create-params rest-var-arrow-center alist (car new-arrow) (cdr new-arrow)))))))
				; the end of the string
				((equal? next-string "")
						(cons param-string param-list))))))
	
	
(define convert-string
	(lambda (obj)
		(if (string? obj)
			obj
			(format "~a" obj))))
	
(define convert-alist-to-strings
	(lambda (alist)
		(if (null? alist)
			'()
			(let ((key (caar alist))
				  (value (cadar alist)))
				(cons (list (convert-string key) (convert-string value)) (convert-alist-to-strings (cdr alist)))))))

(define valid?
	(lambda (str)
		(<valid-string> (string->list str)
			(lambda (match remaining)
				#t)
			(lambda (_)
				#f))))

(define has-templates?
	(lambda (str)
		(<is-template> (string->list str)
			(lambda (match remaining)
				#t)
			(lambda (_)
				(<st-or-comment> (string->list str)
					(lambda (match remaining)
						(if (equal? match "")
						#f
						(if (null? remaining)
							#f
							(has-templates? (list->string remaining)))))
					(lambda (_)
						#t))))))
			
;; this function receives a string and some optional parameters
;; the optional arguments will be held in a list 'lst'
(define formatter 
	(lambda (str . lst)
		(if (not (valid? str))
			"Error: Input string is not valid"
			(cond ((not (has-templates? str)) (car (get-next <st> str))) ; no templates in the input string
				  ((null? lst) ("Error: Not enough values")) ; str has templates but no values
				  (else
					(let ((params (create-params str (convert-alist-to-strings (car lst)) "" '())))
						(if (equal? params #f)
							"Error: Not enough values"
							(apply format params))))))))
