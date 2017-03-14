(load "pattern-matcher.scm")
(load "pc.scm")

;*****************************************************************************************************************************************************
;;Assignment1

(define <digit-0-9>
  (range #\0 #\9))

(define <char-a-f>    
  (range #\a #\f))
  

(define <spacee>
  (new (*parser (char (integer->char 32)))
       *star
       done)) 
(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <LineComment>
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

(define <SexprComment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <InfixExpression>))
       (*delayed (lambda () <sexpr>))
       (*disj 2)
       (*caten 2)
       
       done))

(define <Comment>
  (disj <LineComment>
	<SexprComment>))

(define <skip>
  (disj <Comment>
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

(define ^<skipped*> (^^<wrapped> (star <skip>)))

(define add-list
  (lambda (s)
    (fold-right
     (lambda (a b) (+ a b))
     0
     s)))
  
;***************************************************************************************************************************************
;<Boolean>


(define <Boolean>
  (new 
	(*parser <spacee>)
	(*parser (word-ci "#t"))
	(*parser <spacee>)
	(*caten 3)
       (*pack-with (lambda (s1 a s2) #t))
	(*parser <spacee>)
	(*parser (word-ci "#f"))
	(*parser <spacee>)
       (*caten 3)
       (*pack-with (lambda (s1 a s2) #f))
       (*disj 2)
       done))
       
;***************************************************************************************************************************************
;;<Char>

   
(define <CharPrefix>
  (new (*parser (word "#\\"))
       (*pack
	(lambda (_)
	  "#\\"))
       done))

(define <GreaterThanSpace>
  (range #\! #\~))
  
(define <VisibleSimpleChar>   ;; Pack as String
  (new (*parser <GreaterThanSpace>)
       (*pack
	(lambda (char)
	   `(,@char)))
       done))
       
(define <WhiteSpace>
    (new (*parser <any-char>)
        (*parser <GreaterThanSpace>)
        *diff
        done))
        
(define <WhiteSpace-Star>
    (new (*parser <WhiteSpace>)
        *star
        done))


(define <NamedChar>   ;PACK AS CHAR
    (new (*parser (word "lambda")) 
        (*pack (lambda(_)
            (integer->char 955)))
    (*parser (word "newline"))
        (*pack (lambda(_)
            (integer->char 10)))
    (*parser (word "nul"))
        (*pack (lambda(_)
            (integer->char 0)))
    (*parser (word "page"))
        (*pack (lambda(_)
            (integer->char 12)))
    (*parser (word "return"))
        (*pack (lambda(_)
            (integer->char 13)))
    (*parser (word "space"))
        (*pack (lambda(_)
            (integer->char 32)))
    (*parser (word "tab"))
        (*pack (lambda(_)
            (integer->char 9)))
    (*disj 7)
 
    done))

(define <HexChar>
  (new (*parser <digit-0-9>)
       (*parser <char-a-f>)
       (*disj 2)
       (*pack 
	  (lambda (a)
	  a))
       done))

       
(define <HexUnicodeChar>  ;; PACK AS SYMBOL 
  (new (*parser (char #\x))
       (*parser <HexChar>) *plus
       (*caten 2)
       (*pack-with
	(lambda (x hex)
	   (string->number
	  (string-append "#" (list->string `(,x ,@hex))))
	  ))
       done))
       
(define <Char>    
  (new
  (*parser <CharPrefix>)
  (*parser <HexUnicodeChar>)
  (*parser <NamedChar>)
  (*parser <VisibleSimpleChar>) 
       (*disj 3)
       (*caten 2)
      (*guard (lambda (Check)
          	 (if (number? (car (cdr Check)))
          	 	(< (car (cdr Check)) 1114111)
          	 	#t)))
       (*pack-with
	(lambda ( prefix char)
	(if (number? char)
	  (integer->char char)
	  char)))
	  
  done))
   


   
;**********************************************************************************************************
;;<Number>

(define <Natural>
  (new
   (*parser <digit-0-9>) *plus
   (*pack
    (lambda (nat)
      (string->number
       (list->string
	`(,@nat)))))
done))

       
(define <Integer>
  (new 
    (*parser <spacee>)
    (*parser (char #\+))
    (*parser <Natural>)
    (*parser <spacee>)
       (*caten 4) ;parser for positive numbers with + before the number
       (*pack-with
	(lambda (s1 ++ n s2) n) )
	 (*parser <spacee>)
       (*parser (char #\-))
       (*parser <Natural>)
       (*parser <spacee>)
       (*caten 4) ; parser for negative numbers with - before the number
       (*pack-with
	(lambda (s -- n s1) (- n)))
      (*parser <spacee>)
     (*parser <Natural>);parser for positive numbers with out a sign before them
        (*parser <spacee>)
        (*caten 3)
       (*pack-with
	(lambda ( s1 n s2) n))
	
       (*disj 3)
       done))
       

(define <Fraction>

  (new 
  (*parser <Integer>)
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
;******************************************************************************************************
;;<String>     

		
(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
	 (*pack (lambda (_) ch))
	 done)))
	 
(define <StringLiteralChar>
  (new (*parser <any-char>)
       (*parser (char #\\))
       *diff
       (*pack 
       (lambda (ch)
            ch))
       done))

(define <StringMetaChar>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page)) ; formfeed
       (*parser (^<meta-char> "\\{lambda}" (integer->char 955)))
       (*disj 7)
       done))
       
(define <StringHexChar>
    (new (*parser (char #\\))
	  (*parser (char #\x))
	  (*parser <HexChar>) *star
	  (*parser (char #\;))
	  (*caten 4)
	  (*pack-with 
	  (lambda (a s d c)
	  (if (<  (string->number (string-append "#" (list->string `(,s ,@d)))) 1114111)
		(integer->char (string->number (string-append "#" (list->string `(,s ,@d)))))
		#f
	  ) ))
	  
	done))
     
(define <StringChar>
  (new (*parser <StringMetaChar>)
	(*parser <StringHexChar>)
       (*parser <StringLiteralChar>)
       (*disj 3)
      done))
       
       
(define <String>
  (new 
	(*parser (char #\"))
       (*parser <StringChar>) 
       (*guard (lambda (check)
	    (if  (equal? check #f)
		  #f
		  #t)))
       (*parser (char #\"))
	*diff
	*star
	(*parser (char #\"))
       (*caten 3)
       (*pack-with
	(lambda ( open chars close)
	 (list->string chars)))
	
 
       done))

      
;******************************************************************************************************************
;<Symbol>

(define <SymbolChar>
  (new (*parser <digit-0-9>)
       (*parser (range #\A #\Z))
       (*pack (lambda (char)
	  (char-downcase char)))
       (*parser (range #\a #\z))
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
  (*parser <spacee>)
  (*parser <SymbolChar>)  
  *plus
  (*parser <spacee>)
  (*caten 3)
  (*pack-with
    (lambda (st chars en)
     ( string->symbol (list->string chars))))
     done))
       
;************************************************************************************************************************
;<List>  
 

(define <ProperList>
       (new 
        (*parser (char #\())
        (*delayed  (lambda() <Number>))
 	(*delayed  (lambda() <sexpr>)) ; allow recursion
 	(*disj 2)
 	(*parser (char #\))) 
 	    *diff
 	     *star
        (*pack (lambda( sec ) 
            `(,@sec)))
          
	(*parser (char #\)))
 	 (*caten 3)
 	   (*pack-with
 	(lambda ( open chars close )
 	   chars))
      done))
 	   
 	   
    (define <ImproperList>
       (new 
       (*parser <spacee>)
       (*parser (char #\()) ; open brackets
	    (*parser <spacee>)
 	   (*delayed  (lambda() <sexpr>))    ; one or more elements
	    (*parser (char #\))) ; not including brackets
 	    *diff
 	   (*parser <spacee>)
 	    (*caten 3)
             (*pack-with (lambda(fir sec c) sec))
             *plus
            (*parser (char #\.)) ; pair symbol
             (*parser <spacee>)
            (*delayed  (lambda() <sexpr>))      ; cooder of pair
              (*parser <spacee>)
            (*caten 3)
           (*pack-with (lambda(fir sec c) sec))
 	   (*parser (char #\)))   ; not including brackets
 	    *diff
 	   (*parser (char #\)))   ; close brackets
	    (*parser <spacee>)
 	   (*caten 7)
 	   (*pack-with
 	(lambda (s0 open ls dot listcdr close s1)
	(append ls  listcdr)))
	done ))	
        
	   



;************************************************************************************************************************
;<Vector>

(define <Vector> 
(new 
    (*parser <spacee>)
    (*parser (char #\#))
     (*parser <ProperList>)
     (*parser <spacee>)
     (*caten 4)
     (*pack-with (lambda(fi a b en)
	  (list->vector b)))
    done))
    
         
;************************************************************************************************************************
;<Quoted>

(define <Quoted>
    (new (*parser <spacee>)
         (*parser (char #\'))
         (*delayed (lambda() <sexpr>))
         (*parser <spacee>)
         (*caten 4)
         (*pack-with
            (lambda (startspace q chars endspace)
	      (list 'quote chars)))
   done))

(define <QuasiQuoted>
    (new (*parser <spacee>)
         (*parser (char #\`))
         (*delayed (lambda() <sexpr>))
         (*parser <spacee>)
         (*caten 4)
          (*pack-with
            (lambda (startspace q chars endspace)
	      (list 'quasiquote chars)))
    done))
    
(define <Unquoted>
    (new (*parser <spacee>)
         (*parser (char #\,))
         (*delayed (lambda() <sexpr>))
         (*parser <spacee>)
         (*caten 4)
	   (*pack-with
            (lambda (startspace q chars endspace)
	      (list 'unquote chars)))
    done))
    
(define <UnquoteAndSpliced>
    (new (*parser <spacee>)
         (*parser (char #\,))
         (*parser (char #\@))
         (*delayed (lambda() <sexpr>))
         (*parser <spacee>)
         (*caten 5)
          (*pack-with
            (lambda (startspace q strodel chars endspace)
	      (list 'unquote-splicing chars)))
    done))
;************************************************************************************************************************
;<Infix>

  
;<InfixPrefixExtensionPrefix>
(define <InfixPrefixExtensionPrefix>
    (new (*parser (char #\#))
	 (*parser (char #\#))
	 (*parser (char #\%))
	 (*disj 2)
	 (*caten 2)
	 (*pack-with (lambda (first second)
		`(,first ,second)))   ;returns list of 2 chars
    done))
		    
	
(define <InfixSymbol>
    (new 
    (*parser <spacee>)
    (*parser <SymbolChar>) 
	 (*parser (char #\*))
	 (*parser (char #\*))
	 (*caten 2)
	 (*parser (char #\+))
	 (*parser (char #\-))
	 (*parser (char #\*))
	(*parser (char #\^))
	 (*parser (char #\/))
	 (*disj 6)
	 *diff
	 
	 *plus
	 (*caten 2)
	 (*pack-with (lambda (s1 sym)	

		(string->symbol (list->string 
		(map char-downcase sym)
		))))
  done))
    


(define help_function (lambda (first operands ls)
		(cond ( (null? operands) ls)
		      ( (list? operands)
		  (if  
		 (and (list? (caar operands)) (= (length (caar operands)) 2 ))
		  (append '(expt) `(,first)  `(,(help_function  
		  
		  (car (cdr (car operands)))
		  (cdr operands) 
		  `( ,@ (car (cdr (car operands)))))) )
		
		    (cond                                  
		  (
		  (= (char->integer (caar operands) ) 94)   ;^
		   
		  (append '(expt) `(,first)  `(,(help_function  
		  
		  (car (cdr (car operands)))
		  (cdr operands) 
		  `( ,@ (car (cdr (car operands)))))) )
		  ) ;^
		   
		    ( (= (char->integer (caar operands) ) 43)  ;+
		  (help_function 
		  (append '(+) `(,first) (cdr (car operands))) ;first
		  (cdr operands)  
		  (append '(+) `(,first) (cdr (car operands)))) )
		
		( (= (char->integer (caar operands) ) 45)  ; -
		  (help_function  
		  (append '(-) `(,first) (cdr (car operands)))
		  (cdr operands)
		  (append '(-) `(,first) (cdr (car operands))))
		)
		
		 ( (= (char->integer (caar operands) ) 42) 
		  (help_function  
		  (append '(*) `(,first) (cdr (car operands)))
		  (cdr operands)
		  (append '(*) `(,first) (cdr (car operands))))
		)
		
		 ( (= (char->integer (caar operands) ) 47) ;/
		  (help_function  
		  (append '(/) `(,first) (cdr (car operands)))
		  (cdr operands)
		  (append '(/) `(,first) (cdr (car operands))))
		)
	)
)))))
		
	  
(define <InfixSubAdd>
     (new 
     (*delayed (lambda() <InfixDivMul>))
	  (*parser <spacee>)
	  (*parser (char #\-))
	  (*parser (char #\+))
	  (*disj 2)
	  (*parser <spacee>)
     (*delayed (lambda() <InfixDivMul>))
	      (*parser <spacee>)
	  (*caten 4)
	  (*pack-with (lambda (op s operand ss)  
			    
			      (list op operand)))	
	  *star	 
	  (*parser <spacee>)
	  (*caten 4)
	  (*pack-with (lambda (first s op_and_operand q)
	
	     (help_function first op_and_operand `(,@first))))
	done))
	

  
(define <InfixDivMul>
     (new (*delayed (lambda() <InfixPow>))
	  (*parser <spacee>)
	  (*parser (char #\*))
	  (*parser (char #\/))
	  (*disj 2)
	  (*parser <spacee>)
          (*delayed (lambda() <InfixPow>))
	  (*parser <spacee>)
	  (*caten 4)
	  (*pack-with (lambda (op s operand ss)  
			    
			      (list op operand)))	
	  *star	
	  (*parser <spacee>)
	  (*caten 4)
	  (*pack-with (lambda (first s op_and_operand q)
	
	     (help_function first op_and_operand `(,@first))))
	done))

(define <PowerSymbol>
    (new 
	(*parser (char #\*))
	(*parser (char #\*))
	(*caten 2)
	(*pack-with (lambda (f s)
	  (list f s)))
	(*parser (char #\^))
	(*pack (lambda (po) 
	po))
	(*disj 2)
	
	done))
	
(define <InfixPow>
   (new 	
   	       (*delayed (lambda() <InfixArrayOrFunc>))
   	   
	 (*parser <spacee>)
	 (*parser <PowerSymbol>)
	 (*parser <spacee>)
	 	     (*delayed (lambda() <InfixArrayOrFunc>))
	 (*parser <spacee>)
	 (*caten 4)
	 (*pack-with (lambda (op s operand ss)  
			      (list op operand)))
	*star 
	(*parser <spacee>)
	(*caten 4)
	(*pack-with (lambda (first s op_and_operand q)
	     (help_function first op_and_operand `(,@first))))
	
	done))

;InfixNeg
(define <InfixNeg>
   (new 
  (*parser <spacee>)
  (*parser (char #\-))
   (*parser <spacee>)
   	 (*delayed (lambda() <Number>))
   	 (*delayed (lambda() <InfixArrayOrFunc>))
	 (*delayed (lambda() <InfixSymbol>))
	 (*delayed (lambda() <InfixExpression>))
	 (*disj 4)
	 (*caten 4)
	 (*pack-with (lambda (s2 minu s1 inf)
	  (cond
	  ( (number? inf) 
	  (if (not (null? s1))
	  
					 (list '-  5) 
					  (- inf) ) )    
	    (else  (list '- inf)))))
	done))
	

(define HelpRec 
      (lambda (first operands ls)
	    (cond ( (or (null? operands)   (not (list? operands))) 
	    ls)
		  (   (and (not (null? operands))(list? operands ))
			(if (equal? (caar operands) 'brackets)
			(HelpRec (list 'vector-ref first (cdar operands) ) (cdr operands)   (list 'vector-ref first  (cdar operands) ))	   

			(if (and (not (null? operands)) (null? (car (cdr (car operands))) )) (list first)
			(HelpRec (append (list first) (cadr (car operands)) ) (cdr operands) (append (list first) (cadr (car operands)) ))))
		)))   )
		    
	
	
(define <InfixArrayOrFunc>
     (new 
      (*delayed (lambda() <LastOne>))
      
      (*parser (char #\[ ))
      (*delayed (lambda() <InfixSubAdd>))      
      (*parser (char #\] ))
      (*caten 3)
      (*pack-with (lambda (a b c)
      
 	(cons 'brackets b)))	   
	;;InfixFuncall
       (*parser (char #\( ))
       (*delayed (lambda() <InfixArgList>))      
       (*parser (char #\) ))
       (*caten 3)
       (*pack-with (lambda (a b c)
  	(list 'u b) ))
  	(*disj 2)
    	 *star    
         (*caten 2)
      (*pack-with (lambda (first op_and_operand)
	(HelpRec first op_and_operand first)
	  	   ))
    done)) 
    
    
	
(define <LastOne>
    (^<skipped*> (new 
    (*delayed (lambda() <InfixSexprEscape>))
     (*delayed (lambda() <InfixParen>))
      (*parser <InfixNeg>)	  
      (*parser <Number>)
      (*parser <InfixSymbol>)
      *not-followed-by
      (*parser <InfixSymbol>)
	  (*disj 5)
	done)))

(define <InfixParen>
  (new
    (*parser (char #\( ))
    (*delayed (lambda() <InfixExpression>))
    (*parser (char #\) ))
    (*caten 3)
    (*pack-with 
	(lambda (op ex cl) ex) )
   done ))

(define <InfixArgList> 
     (new 
     (*parser <spacee>)
     (*delayed (lambda() <InfixExpression>))
       (*parser <spacee>)
        (*parser (char #\,))
     (*delayed (lambda() <InfixExpression>))
	(*caten 2)
	(*pack-with (lambda (comma inf)
	      inf))
	 *star
	 (*caten 4)
	 (*pack-with (lambda (s1 inf s2 args)
	    `(,inf ,@args)))
	(*parser <spacee>)
	(*parser <epsilon>)
	(*parser <spacee>)
	(*caten 3)
	(*pack-with (lambda (s1 ep s2)
	ep))
	(*disj 2)
	done)) 
	
(define <InfixSexprEscape>
      (^<skipped*>
      (new 
	(*parser <InfixPrefixExtensionPrefix>)
     (*delayed (lambda() <sexpr>))
     (*caten 2)
     (*pack-with (lambda (pre sexpr)
	  sexpr))
	  done)))
	
    ;<InfixExpression>
(define <InfixExpression>
    (new 
   (*parser <InfixSubAdd>)
    (*parser <Number>)
    (*parser <InfixSymbol>)
    (*parser <InfixParen>)
    (*disj 4)
    (*pack (lambda ( inf ) 
	inf))
    done))
	

(define <InfixExtension>
    (new 
    (*parser <InfixPrefixExtensionPrefix>)
    (*parser <InfixExpression>)
      (*caten 2)
	 (*pack-with (lambda (pre inf)
	      inf)) 
     done))

	  
(define <sexpr>
      (^<skipped*>
	(new
	 (*parser <Boolean>)
	 (*parser <Number>)
	 (*parser <Symbol>)
	 *not-followed-by
	 (*parser <Symbol>)
	 (*parser <Char>)
         (*parser <String>)
         (*parser <ProperList>)
         (*parser <ImproperList>)
         (*parser <Vector>)
         (*parser <Quoted>)
         (*parser <Unquoted>)
         (*parser <QuasiQuoted>)
         (*parser <UnquoteAndSpliced>)
         (*parser <InfixExtension>)
         (*disj 13) 
    done)))
(define <Sexpr> <sexpr>)

;*****************************************************************************************************************************************************;
;;Assignment2
(define identify-lambda
	(lambda (argl ret-simple ret-opt ret-var)
		(cond 
			((null? argl) (ret-simple '()))
			((var? argl) (ret-var argl))      ;;;TODO: var?
			(else (identify-lambda (cdr argl)
					(lambda (s) (ret-simple `(,(car argl) ,@s))) ;simple
					(lambda (s opt) (ret-opt `(,(car argl) ,@s) opt)) ;opt
					(lambda (var) (ret-opt `(,(car argl)) var))))))) 

					
(define *reserved-words*
      '(and begin cond define do else if lambda
      let let* letrec or quasiquote unquote
      unquote-splicing quote set!))


					
(define var? 
    (lambda (var)
      (and (symbol? var)
      (not (member var *reserved-words*)))
     ))
	  
(define val? 
    (lambda (val)
    #t))
      
(define simple-const?
      (lambda (constant)
	  (or (vector? constant) (boolean? constant) (char? constant) (number? constant) (string? constant))
	  ))
	  
(define df?
    (lambda (lstBinding)
	(andmap (lambda (ls) (= (length (filter (lambda (x) (eq? (car x) (car ls))) lstBinding)) 1)) lstBinding))) 
	  
(define beginify
	(lambda (s)
		(cond
			((null? s) void-object)
			((null? (cdr s)) (car s))
			(else `(begin ,@s)))))
			
(define with (lambda (s f) (apply f s)))

(define letrec? (lambda (expr)
	(and (list? expr) (equal? (car expr) 'letrec))))

	
	
	;;;;;;;;;;;;;;;Mayer code;;;;;;;;;;;;;;;
(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
	 (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
		 simple-sexprs-predicates)
	  (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

(set! a 0)


(define correctLambda
    (lambda (args)
	(map (lambda (x) (map (lambda (y) (if (equal? x y) (set! a (+ a 1)) (set! a a)) ) args) ) args)
	
	(if (= (length args) a) #t #f) 
	    
	    ))
	    
(define correctLam? 
  (lambda (args)
    (begin (set! a 0) (correctLambda args) )))

    (define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Mayer code;;;;;;;;;;;;;;;;;;;;;;;;;

(define Loop
  (lambda (lst)
  (if (null? lst)
      '()
    (if (and (list? (car lst) ) (not (null? (car lst))) (equal? (caar lst) 'begin))
	  (append (cdar lst) (Loop (cdr lst)))
	  (append `(,(car lst)) (Loop (cdr lst)))))))
	
(define FixedPoint
    (lambda (lst1)
	(if (equal? (Loop lst1) lst1) lst1 (FixedPoint (Loop lst1)))))

(define parse
	(let ((run 
			(compose-patterns
			
				

				;constant
				(pattern-rule
					(? 'c simple-const?  )
					(lambda (c) `(const ,c)))
				(pattern-rule
					`(quote ,(? 'c) )
					(lambda (c) `(const ,c)))
				;;var
				(pattern-rule
					(? 'v var?)
					(lambda (v) `(var  ,v)))
				;;conditionals
				(pattern-rule
					`(if ,(? 'test) ,(? 'dit))
					(lambda (test dit) `(if3 ,(parse test) ,(parse dit) (const ,(void)))))
				(pattern-rule
					`(if ,(? 'test) ,(? 'dit) ,(? 'dif))
					(lambda (test dit dif) `(if3 ,(parse test) ,(parse dit) ,(parse dif))))
				;;or
				(pattern-rule
					`(or ,@(? 'exprs list?))
					(lambda (lst) 
					    
					    
					(if (null? lst)
					    `(const #f)
					(if (= (length lst) 1)
					  (parse (car lst))
					`(or ,(map parse lst) )))))
			
				;; lambda
				(pattern-rule 
					`(lambda ,(? 'args) ,@(? 'body list?))
					(lambda (args body)
 					   (identify-lambda args  
 					    (lambda (s) (if (correctLam? args) `(lambda-simple ,s ,(parse (beginify body)) )(error 'parse
									(format "I can't recognize this: ~s" e))  ))
 					    (lambda (s opt) `(lambda-opt ,s ,opt ,(parse (beginify body))))
 					    (lambda (var) `(lambda-var ,var ,(parse (beginify body)) ) ))))
				
				;;regular define 
				(pattern-rule 
					 `(define  ,(? 'v  var?) ,(? 'expr))
					 (lambda (v expr)
						`(def ,(parse v) ,(parse expr))) )
				;;MIT-style define
				(pattern-rule 
					 `(define ,(? 'args) ,@(? 'expr))
					  (lambda (args expr)
						`(def  ,(parse (car args)) 
						,(identify-lambda (cdr args)  
 					    (lambda (s) (if (correctLam? args) `(lambda-simple ,s ,(parse (beginify expr)) )(error 'parse
									(format "I can't recognize this: ~s" e))  ))
 					    (lambda (s opt) `(lambda-opt ,s ,opt ,(parse (beginify expr))))
 					    (lambda (var) `(lambda-var ,var ,(parse (beginify expr)) ) )))
 					     ))
 					     
 				;;sequence 
 				(pattern-rule
					  `(begin . ,(? 'ls list?))
					      (lambda (ls)
					      (cond ( (null? ls) `(const ,(void)))
						    ( (= 1 (length ls) ) (parse (car ls)))
						    (else `(seq ,(map parse (FixedPoint ls)))))))
 					 
 					 
				;;3.2 Macro Expansions
 			
				;;let 
				(pattern-rule
					`(let ,(? 'valexpr list?)  ,@(? 'body) )
					(lambda (valexpr body)
					(parse `(
					(lambda ,(map (lambda (a) (car a)) valexpr) ,@body )
					,@(map (lambda (a) (car (cdr a)))  valexpr)
					)
					)))
					
					
				
					
				;;let* 
				(pattern-rule
					`(let* () ,(? 'expr) . ,(? 'exprs list?))
					(lambda (expr exprs) (parse  `(let () ,@(cons expr exprs)))))
				
				(pattern-rule
					`(let* ((,(? 'var var?) ,(? 'val val?)) . ,(? 'rest)) . ,(? 'exprs))
					(lambda (var val rest exprs) (parse `(let ((,var ,val)) ,(if (null? rest)  (beginify (cons (car exprs) (cdr exprs))) `(let* ,rest . ,exprs))))))
					
	
			
				;set!
				(pattern-rule
					`(set! ,(? 'v var?) ,(? 'val))
					(lambda (v val)
					      `(set ,(parse v) ,(parse val)))) 
				;;letrec	
				(pattern-rule 
					(? 'exprs letrec? (lambda (lst) (df? (cadr lst)))) 
					  (lambda (exprs)
					      (parse `(let 
					      ,(map (lambda (a) `(,(car a) #f)) (car (cdr exprs)) ) ;;binding vars
 					      ,@(map (lambda (a) `(set! ,(car a) ,(cadr a)) ) (car (cdr exprs)) ) ;binding values
					      (let () ,@(cdr (cdr exprs))))
					    ) ))
				;;and 
				(pattern-rule   ;;0	arguments
				      `(and)
					  (lambda () (parse #t)))
					  
				(pattern-rule 
				      `(and ,(? 'exprs)) ;1 arguments
					  (lambda (exprs) (parse exprs)))
				
				(pattern-rule 
					`(and ,(? 'exprs) . ,(? 'rest)) ;2+ arguments
					    (lambda (exprs rest)
						  (parse `(if  ,exprs (and ,@rest) #f))))
				
				;;cond
				
				
				(pattern-rule   ;;else
				    `(cond (else . ,(? 'exprs )))
					(lambda (exprs)
					    (parse (beginify exprs) )))
			
				(pattern-rule  
				  `(cond (,(? 'expr) . ,(? 'action)) . ,(? 'rest))
				    (lambda (expr action rest)
				     (if (null? rest) 
				      (parse `(if ,expr ,(beginify action)))
				      (parse `(if ,expr ,(beginify action) (cond ,@rest)))))
				)
				
				;quasiquote	      
				(pattern-rule 
				    (list 'quasiquote (? 'exprs))
				      (lambda (exprs)
					  (parse (expand-qq exprs))))
				      
					     
					
				;;applic
				(pattern-rule
					`(,(? 'operator) ,@(? 'exprs list?) )
					  (lambda (var lst) 
					     `(applic ,(parse var) ,(map parse lst))))
			
				)))
			(lambda (e)
				(run e
						(lambda ()
							(error 'parse
									(format "I can't recognize this: ~s" e)))))))
									
									
;;******************************************************************************************************************************************************
;;Assignment3
;pes : parsed exprs
;ds - defintions
;es - expressions



(define helper-AH (lambda (lst) 

            ( helper-eliminate-AH (eliminate-nested-defines1 lst (lambda (x y)
            (if (null? x) 
		lst 
	      `(,(transform-p-letrec-to-lambda `(letrec ,(mymap (lambda (i)  `(,(cadr (cadr i)) ,(caddr i))) x) ,@y)))))))
))

    (define helper-eliminate-AH (lambda (lst)     
 (mymap (lambda (x)    
 
    (if  (or (null? x) (not (list? x)))  
                 x
                 (if (or (is_lambda_simple? x) (is_lambda_var? x))
                           `(,(car x) ,(cadr x) ,@(helper-AH (cddr x)))
                 (if (is_lambda_opt? x)
                           `(,(car x) ,(cadr x) ,(caddr x) ,@(helper-AH (cdddr x)))
                 
                 (helper-eliminate-AH x)   )
 
 ) )) lst

)))
    (define helper-eliminate-AH (lambda (lst)     

 (mymap (lambda (x)    
 
    (if  (or (null? x) (not (list? x)))     
                 x
                 (if (or (equal? (car x) 'lambda-simple) (equal? (car x) 'lambda-var))
                           `(,(car x) ,(cadr x) ,@(helper-AH (cddr x)))
                 (if (equal? (car x) 'lambda-opt)
                           `(,(car x) ,(cadr x) ,(caddr x) ,@(helper-AH (cdddr x)))
                 
                 (helper-eliminate-AH x)   )
 
 ) )) lst

)))
    
    

(define eliminate-nested-defines (lambda (lst) 
      (car (helper-eliminate-AH `(,lst)))
))

      
(define eliminate-nested-defines1
  (lambda (pes ret-ds+es)
      (if (null? pes) (ret-ds+es '() '())
	    (eliminate-nested-defines1 (cdr pes)
		  (lambda (ds es)
			  (cond ( (eq? (caar pes) 'def)
				  (ret-ds+es (cons (car pes) ds ) es))
				  ( (eq? (caar pes) 'seq)
				      (eliminate-nested-defines1 (cadar pes)
				      (lambda (ds1 es1 ) (ret-ds+es
				      (append ds1 ds)
				      (append es1 es)))))
				 (else  (ret-ds+es ds (cons (car pes) es))))
      )))))

;;building the letrec and parsing it to by ass2
(define build-letrec-from-ds-es
  (lambda (x y)
            (if (null? x) 
		lst 
	      `(,(transform-p-letrec-to-lambda `(letrec ,(my-map (lambda (i)  `(,(cadr (cadr i)) ,(caddr i))) x) ,@y))))))
	

	
(define create_set_exprs
    (lambda (varss_ valuess_ lst) ; Assume varss and valuess are of the same length
        (cond  ( (null? varss_) lst)
               ( else (create_set_exprs (cdr varss_) (cdr valuess_) (append lst (list 
               
               `(set (var ,(car varss_)) ,@(car valuess_))
               
               )))))))	
               
(define create_falsee_ 
    (lambda (varss_)
        (map (lambda (x) `(const #f)) varss_)))

(define transform-p-letrec-to-lambda 
    (lambda (letrec-exp)
        (let* ( (bindings_ (cadr letrec-exp))
                (body1_  (cddr letrec-exp))
                (varss_ (map car bindings_))
                (create_false_ (create_falsee_ varss_))
                (valuess_ (map cdr bindings_))
                (set_seq  `(seq (,@(create_set_exprs varss_ valuess_ '()) ,@body1_) ))
                ;; function that return list of seqences of sets of the varss and the valuess...
                (lambda_expss `(lambda-simple ,varss_ ,set_seq ))
                (applic_exp `(applic ,lambda_expss ,create_false_)  )
                )
               applic_exp
        
                )))
                	 
	 
	 

;************************************************************************************************************


                
;****** remove-applic-lambda-nil 
;help functions        
(define is_lambda_simple?
    (lambda (exps)
      (eq? (car exps) 'lambda-simple)))

(define is_lambda_opt?
    (lambda (exps)
      (eq? (car exps) 'lambda-opt)))
      
(define is_lambda_var?
    (lambda (exps)
      (eq? (car exps) 'lambda-var)))
;bodies     
(define get_body_of_lambda_simple
    (lambda (lam_exps)
	(caddr lam_exps)))
	
(define get_body_of_lambda_opt
    (lambda (lam_exps)
	(cadddr lam_exps)))

(define get_body_of_lambda_variadic
    (lambda (lam_exps)
	(caddr lam_exps)))
;;parms
(define lambda_simple_paramters
  (lambda (lam_exps)
    (cadr lam_exps)))
    
(define lambda_opt_paramters
  (lambda (lam_exps)
   (append (cadr lam_exps) (list (caddr lam_exps)))))
   
(define lambda_variadic_paramters
  (lambda (lam_exps)
    (list (cadr lam_exps))))
    
(define is_applic?
  (lambda (exps)
      (eq? (car exps) 'applic)))
      
(define get_apllic_first
    (lambda (exps)
	 (cadr exps) ))
(define is_lambda?
(lambda (pes)
    (or (eq? (car pes) 'lambda-simple) (eq? (car pes) 'lambda-opt) (eq? (car pes) 'lambda-var))))

	 
(define get_apllic_args_lst
    (lambda (exps)
	 (cddr exps)))
;;Part 4	 
(define remove-applic-lambda-nil
  (lambda (lst)
    (car (remove-applic-lambda-nil1 (list lst)))))

    
(define remove-applic-lambda-nil1 
    (lambda (pes )
     (map (lambda (y)
	    (if (or (null? y) (not (list? y) ) )
		  y
		 (if (and (is_applic? y) (is_lambda_simple? (get_apllic_first y)) (null? (lambda_simple_paramters (get_apllic_first y))))
	 (car (remove-applic-lambda-nil1 (list (get_body_of_lambda_simple (get_apllic_first y)) )))
    (remove-applic-lambda-nil1 y)
 )))pes)))
	      
	     
	      
;;**part 5

(define (Notmember? x list)
     (if (null? list) #t                                
         (if (equal? x (car list)) #f                  
              (Notmember? x (cdr list))))) 
        

 
(define var_in_lambda_params (lambda (lst-lambda var)
      (cond ((equal? (car lst-lambda) 'lambda-simple)  (Notmember? (cadr var) (lambda_simple_paramters lst-lambda) ))      
            ((equal? (car lst-lambda) 'lambda-var)  (Notmember? (cadr var) (lambda_variadic_paramters lst-lambda) ))
            ((equal? (car lst-lambda) 'lambda-opt)  (Notmember? (cadr var) (lambda_opt_paramters lst-lambda) ))
            )
      ))
 
 (define is_Bound?
  (lambda (lambda_body var)
  
 (is_check_if_bound1? lambda_body var 0)  ))
       
        
  (define is_check_if_bound1? (lambda (lambda_body var level)       
                     (ormap (lambda (x)    
 
                          (if  (or (null? x) (not (list? x)))
                                                    #f
                          (if (and (equal? x var) (> level 0))
                                       #t
                                       
                                     
                 (if  (is_lambda? x)
                         (if (not (var_in_lambda_params x var))
                             #f
                         (is_check_if_bound1?  x var (+ 1 level)))
                        (is_check_if_bound1? x var level))
 
                                  )      ) ) lambda_body)      
      ))
      
      

        
        
; Boxing of variables


 
 
; It is set (via a set!-expression) somewhere in the body of the procedure

(define is_set_?
  (lambda (exps var)
	 ( equal? var ( cadr exps) )))
 


	
(define is_var_set?
      (lambda (lambda_body var)
	(ormap (lambda (y)
	 (if (and (not (null? y)) (list? y))
	
	 (if (and (eq? 'set (car y)) (is_set_? y var)) #t
	 
		  (is_var_set?  y var ))
		      #f
		  )) (car (list lambda_body)) )
		  
		  
	))
	
	
	
	
;It has a get-occurrence somewhere in the body of the procedure. A get means that it either
;appears as a (pvar name minor) or (bvar name major minor).	


	
(define is_get-occurrence_var?
      (lambda (lambda_body var)
	   (ormap (lambda (y)
	 (if (and (not (null? y)) (list? y))
	 
	 (if (equal? var y) #t
	     (if  (eq? 'set (car y))   
	                 ( is_get-occurrence_var? (cddr y)  var)
	     (if (and (is_lambda? y) (not (var_in_lambda_params y var)))
	                       #f
		  (is_get-occurrence_var?  y var ))))
		  #f
		  )) (car (list lambda_body)) )
		  
	   
	   ))
	   
(define change_to_box-get
    (lambda (lambda_body var)
     
	(map (lambda (y)
	 	 (if (and (not (null? y)) (list? y))
	 	 (if (and (is_lambda? y) (not (var_in_lambda_params y var))) y
		(if (equal? (car y) 'box-set) `(box-set ,(cadr y) ,@(change_to_box-get `(,(caddr y)) var ))       ; 
	 (if  (equal? var y) `(box-get ,y)  
	 (change_to_box-get y var ))))
	 y
	   
		  ) )lambda_body)))
(define change_to_box-set
    (lambda (lambda_body var)
     
	(map (lambda (y)
	 	 (if (and (not (null? y)) (list? y))
		      (if (and (is_lambda? y) (not (var_in_lambda_params y var))) y
	 (if (and (equal? (car y) 'set) (equal? var (cadr y)))
	 `(box-set ,(cadr y) ,(change_to_box-set (caddr y) var))  
	 (change_to_box-set y var )
	 ))
	 
	 y
	   
		  ) )lambda_body)))  

(define helper_Do_box_and_get 
  (lambda (lambds parms body_)
      (cond ( (or (not (list? parms)) (null? parms)) lambds ) 
	      ( (and (is_Bound? body_ `(var ,(car parms)))  (is_var_set? body_ `(var ,(car parms))) (is_get-occurrence_var? body_ `(var ,(car parms))))
	      (helper_Do_box_and_get (change_to_box-get (change_to_box-set lambds `(var ,(car parms))) `(var ,(car parms)))  (cdr parms) body_))
	      
	      (else (helper_Do_box_and_get lambds (cdr parms) body_))
	      )
    ))
  
  
(define box-set_help
      (lambda (lambdas)
	  (let* ( (parama1 (cond ( (eq? (car lambdas) 'lambda-simple)  (lambda_simple_paramters lambdas))
				  ( (eq? (car lambdas) 'lambda-opt) 	(lambda_opt_paramters lambdas))
				  ( (eq? (car lambdas) 'lambda-var)  (lambda_variadic_paramters lambdas))   ))
				  
		  (body_  (cond ( (eq? (car lambdas) 'lambda-simple)  `(,(get_body_of_lambda_simple lambdas)))
				  ( (eq? (car lambdas) 'lambda-opt) 	`(,(get_body_of_lambda_opt lambdas)))
				  ( (eq? (car lambdas) 'lambda-var) `(,(get_body_of_lambda_variadic lambdas)))   ))  
		
		(lambdd (helper_Do_box_and_get  lambdas parama1 body_))
		
		(body_1  (cond ( (eq? (car lambdd) 'lambda-simple)  (get_body_of_lambda_simple lambdd))
				  ( (eq? (car lambdd) 'lambda-opt) 	(get_body_of_lambda_opt lambdd))
				  ( (eq? (car lambdd) 'lambda-var) (get_body_of_lambda_variadic lambdd))   ))
		;this willl give us list of if the three conditions occur ==> (set (pvar name minor) (box (pvar name minor)))	
		(sets_to_add_after_lambda 
		(filter (lambda (t) (not (null? t))) (mymap 
					    (lambda (p)
		      (if (and  (is_Bound? body_ `(var ,p))  (is_var_set? body_ `(var ,p)) (is_get-occurrence_var? body_ `(var ,p))) 
			      `(set (var ,p) (box (var ,p)))
			      '() )
				    ) parama1 )
				 ))
		)

		((lambda (xxx) 
		    (cond ( (is_lambda_var? xxx)
		    `( ,(car xxx) ,@parama1 ,(caddr xxx)))
			((is_lambda_opt? xxx)
			`( ,(car xxx) ,(reverse (cdr (reverse parama1))) ,(car (reverse parama1)) ,(caddr xxx)) )
		    ( (is_lambda_simple? xxx)
		    `( ,(car xxx) ,parama1 ,(caddr xxx)))
		
		))
		;;now we want to add 
		(if (> (length sets_to_add_after_lambda)  0)
		(if (equal? (car body_1) 'seq)
		 `( ,(car lambdd) ,parama1   (seq (,@sets_to_add_after_lambda ,@(cadr  body_1))))
		`( ,(car lambdd) ,parama1   (seq (,@sets_to_add_after_lambda , body_1))))
		`( ,(car lambdd) ,parama1   , body_1)))
				  
      )))
      
(define box-set_
      (lambda (eliminate_pes)
     (map (lambda (y)
	    (if (or (null? y) (not (list? y) ) )
		  y
		 (if (is_lambda? y) 
		 (box-set_ (box-set_help y)) 
		 
		 (box-set_ y)
	      
	      )))eliminate_pes)))
(define box-set
          (lambda (eliminate_pes)
    (car (box-set_ `(,eliminate_pes)))))

    
;Part 6
;Annotating Variables with their Lexical address

;; overright the mao by mymap
(define (mymap f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (mymap f (cdr lst)))))

            
;;here we implement part 6

;good
(set! c_lambdas_counter -1)		  
(define change_pvar
    (lambda (lambds var level place)
    (mymap (lambda (x)
    (if (and (not (null? x)) (list? x))
	      
	    (if (is_lambda? x ) (change_pvar x  var (+ 1 level) place)
	    (if (and (= 0 level) (equal? x var) )
		`(pvar ,(cadr var) ,place)
		(if (and (not (= 0 level)) (equal? x var) )
		  `(bvar ,(cadr var) ,(-  level 1) ,place)
		  (change_pvar x  var level place)
		  
		)
		))
		 
		  x)
	   ) lambds)
    
	))
	
(define it
  (lambda (lambdas parms number)
    (if (null? parms)
        lambdas
        (if (equal? void parms)
        (it (change_pvar lambdas  `(var ,(car parms)) 0  number) '() (+ 1 number))
    (it (change_pvar lambdas  `(var ,(car parms)) 0  number) (cdr parms) (+ 1 number))
      )))
)


(define pe->lex-pe_help
      (lambda (lambdas)
	  (let* ( (parama1 (cond ( (eq? (car lambdas) 'lambda-simple)  (lambda_simple_paramters lambdas))
				  ( (eq? (car lambdas) 'lambda-opt) 	(lambda_opt_paramters lambdas))
				  ( (eq? (car lambdas) 'lambda-var) (lambda_variadic_paramters lambdas))   ))
	      
		
		;(lambdd (it  lambdas parama1 0))
		
		
		;this willl give us list of if the three conditions occur ==> (set (pvar name minor) (box (pvar name minor)))	
		
		)
	
                (it  lambdas parama1 0)
		 ;lambdd 
	
				  
      )))
      
(define var_toFvar
  (lambda (pes)
     (mymap (lambda (y)
	    (if (or (null? y) (not (list? y) ) )
		  y
		 (if (equal? (car y) 'var) 
		 `(fvar ,(cadr y))
		 (var_toFvar y)
	      
	      )
	      ))pes)))
      
(define pe->lex-pe_rec
      (lambda (pes)
     (mymap (lambda (y)
	    (if (or (null? y) (not (list? y) ) )
		  y
		 (if (is_lambda? y) 
		 (pe->lex-pe_help (pe->lex-pe_rec y))
		 (pe->lex-pe_rec y)
	      
	      )
	      ))pes)))
	      
(define pe->lex-pe
          (lambda (pes)
    (car (var_toFvar(pe->lex-pe_rec `(,pes))))))


;;**part 7
;check_ = 1 ==> then there is tc-applic
;check_ = 0 ==> then there is applic

(define annotate-tc_help
  (lambda (expr check_)
      (cond 
	    ((is_lambda? expr)
             (let ((body  (car (reverse expr)))
                   (with_out_body (reverse (cdr (reverse expr)))))
               `(,@with_out_body ,(annotate-tc_help body 1))))
               
            ((eq? (car expr) 'or)
             (let ((lst_with_out_last (reverse (cdr (reverse (cadr expr)))) )
                   (last_item (car (reverse (cadr expr)))))
               `(or ,(append (map (lambda (x) (annotate-tc_help x 0)) lst_with_out_last) (list (annotate-tc_help last_item check_))))))
               
            ((eq? (car expr) 'if3)
             (let ((test (cadr expr))
                   (dit (caddr expr))
                   (dif (cadddr expr)))
               `(if3 ,(annotate-tc_help test 0) ,(annotate-tc_help dit check_) ,(annotate-tc_help dif check_))))  
               
            ((eq? (car expr) 'def)
               `(def ,(cadr expr) ,(annotate-tc_help (caddr expr) 0)))
               
               
               
            ((eq? (car expr) 'seq)
	    (let ((lst_with_out_last (reverse (cdr (reverse (cadr expr)))) )
                   (last_item (car (reverse (cadr expr)))))
               `(seq ,(append (map (lambda (x) (annotate-tc_help x 0)) lst_with_out_last) (list (annotate-tc_help last_item check_))))))
               
            ((or (eq? (car expr) 'set) (eq? (car expr) 'box-set))
	    (let ((lst_with_out_last (reverse (cdr (reverse (cdr expr)))) )
                   (last_item (car (reverse (cdr expr)))))
               `(,(car expr) ,@(append (map (lambda (x) (annotate-tc_help x 0)) lst_with_out_last) (list (annotate-tc_help last_item 0))))))
               
            ((eq? (car expr) 'applic)
             (if (= check_ 1)
		   `(tc-applic ,(annotate-tc_help (cadr expr) 0) ,(map (lambda (x) (annotate-tc_help x 0)) (caddr expr)))
                 `(applic ,(annotate-tc_help (cadr expr) 0) ,(map (lambda (x) (annotate-tc_help x 0)) (caddr expr)))
                ))
                
            
             (else expr)
                )))

(define annotate-tc
  (lambda (expr)
    (annotate-tc_help expr 0)))



;****************************************************************************************************************************************************
;Project

(define get-last-element
  (lambda (lst)
    (car (reverse lst))))

(define remove-last-element
  (lambda (lst)
    (reverse (cdr (reverse lst)))))
 
(define test-s
  (lambda (parser string) 
(letrec (( test-str
  (lambda (parser string cont)
    (parser (string->list string)
	    (lambda (e s) 
	    (if (null? s) 
	     (append  cont (list e))
	      (test-str parser (list->string s) (append cont (list e)))
		))
	    (lambda (w) `(failed with report: ,@w))))))
	    (test-str parser string '()))))
	    
	    

						      
(define file->string
(lambda (in-file)
(let ((in-port (open-input-file in-file)))
(letrec ((run
(lambda ()
(let ((ch (read-char in-port)))
(if (eof-object? ch)
(begin
(close-input-port in-port)
'())
(cons ch (run)))))))
(list->string
(run))))))   
						   

(define full-parsed
    (lambda (sexpr)
	(annotate-tc (pe->lex-pe (box-set (remove-applic-lambda-nil (eliminate-nested-defines (parse sexpr))))))))

	
; extracts the consts or fvars (depends on the label specified) from the list of parsed expressions 
(define get-consts-or-fvars
  (lambda (tag)
    (letrec ((foo
              (lambda (pe)
                (cond
                 ((atom? pe) '())
                 ((null? pe) '())
                 ((eq? (car pe) tag) (list pe))
                 (else (append (foo (car pe)) (foo (cdr pe))))))))
            
            foo)))
           

(define get-consts (get-consts-or-fvars 'const))

(define get-fvars (get-consts-or-fvars 'fvar))

(define remove-duplicates
  (lambda (lst)
    (letrec ((rem
              (lambda (lst)
                (cond
                 ((null? lst) '())
                 ((member (car lst) (cdr lst))
                  (rem (cdr lst)))
                 (else (cons (car lst) (rem (cdr lst)))))
                 )))
      (reverse (rem (reverse lst))))))
      
(define void?
  (lambda (expr)
    (eq?  void-object expr)))
      
      
; in order for multiple consts to point on the same value
(define topo-sort-consts
 (lambda (expr)
    (cond
      ((or  (null? expr) (boolean? expr)) `(,expr))
      ((or (number? expr) (string? expr) (void? expr)) `(,expr))
      ((pair? expr)
       `(,@(topo-sort-consts (car expr)) ,@(topo-sort-consts (cdr expr)) ,expr))
       ((vector? expr)
        `(,@(apply append
                      (map topo-sort-consts
                           (vector->list expr))) ,expr))
       ((symbol? expr)
        `(,@(topo-sort-consts (symbol->string expr)) ,expr))
       ((char? expr)
        `(,expr))
       (else `(,expr)))))

       
       

(define organize-fvars
  (lambda (fvars)
    (remove-duplicates (apply append (map cdr fvars)))))

    
    
    
(define organize-consts 
  (lambda (consts)

    (remove-duplicates (apply append (map (lambda (const)
                                (remove-duplicates (topo-sort-consts (cadr const))))
                              consts)))))
        
(define Get-associate-i
  (lambda (key lst column)
  ( get-n-element key lst (- column 1))))
                            
(define constants-table
      (lambda (constants_list accumalte_lst addr last_address)
    (cond
     ((null? constants_list) (reverse accumalte_lst))
     (else 
      (let ((current (car constants_list)))
        (cond
         ((and (number? current) (integer? current))
          (constants-table (cdr constants_list)
                        (cons  `(,addr ,current (\T_INTEGER ,current)) accumalte_lst)
                        (+ addr 2)
                        last_address))
         ((and (number? current) (not (integer? current)))
	  (let ( (numerat (numerator current))
		  (denom (denominator current)))
		  
          (constants-table (cdr constants_list)
                        (cons  `(,addr ,current (\T_FRACTION ,numerat ,denom  )) accumalte_lst)
                        (+ addr 3)
                        last_address)) )              
         ((string? current)
          (let ((asci_chr (map char->integer (string->list current))))
            (constants-table (cdr constants_list)
                          (cons `(,addr ,current (\T_STRING ,(string-length current) ,@asci_chr)) accumalte_lst)
                          (+ addr (+ (string-length current) 2))
                          last_address)))
         ((pair? current)
          (let ((addr-car (car (Get-associate-i (car current) accumalte_lst 2)))
                (addr-cdr (car (Get-associate-i (cdr current) accumalte_lst 2))))
            (constants-table (cdr constants_list)
                          (cons `(,addr ,current (\T_PAIR ,addr-car ,addr-cdr)) accumalte_lst)
                          (+ addr 3)
                          last_address)))
         ((symbol? current)
          (let ((addr-str (car (Get-associate-i (symbol->string current) accumalte_lst 2))))
            (constants-table (cdr constants_list)
                          (cons `(,addr ,current (\T_SYMBOL ,addr-str ,last_address)) accumalte_lst)
                          (+ addr 3)
                          addr)))
         ((char? current)
          (constants-table (cdr constants_list)
                        (cons `(,addr ,current (\T_CHAR ,(char->integer current))) accumalte_lst)
                        (+ addr 2)
                        last_address))
         ((vector? current)
          (let ((members (map
                          (lambda (mem)
                            (car (Get-associate-i mem accumalte_lst 2)))
                          (vector->list current))))
            (constants-table (cdr constants_list)
                          (cons `(,addr ,current (\T_VECTOR ,(length members) ,@members)) accumalte_lst)
                          (+ addr 2 (length members))
                          last_address)))
         (else (constants-table (cdr constants_list) accumalte_lst addr last_address)))
        )))))
        

(define create-const-table
    (lambda (pes init-address)
	(let ((basic-consts `(
			   (,init-address  ,void-object (\T_VOID))
			   (,(+ init-address 1) () (\T_NIL))
                           (,(+ init-address 2) ,#t (\T_BOOL 1))
                           (,(+ init-address 4) ,#f (\T_BOOL 0))                       
                           )))
      (constants-table (organize-consts (get-consts pes)) (reverse basic-consts) (+ init-address 6) -1)
      )))
      
(define search-fvar
	(lambda (fvar fvar-tab)
		(cond ((null? fvar-tab) 'error_fvar_not_found)
			  ((equal? (cadar fvar-tab) fvar) (caar fvar-tab))
			  (else (search-fvar fvar (cdr fvar-tab))))))
			
(define fvars->table
   (lambda (fvars accum-lst addr fvars-initss)
     (cond
      ((null? fvars) (reverse accum-lst))
      (else
       (let ((curr (car fvars)))
       (if (Notmember? curr fvars-initss)
         (fvars->table (cdr fvars)
                      (cons `(,addr ,curr) accum-lst)
                      (+ addr 1) fvars-initss)
                      (fvars->table (cdr fvars) ;; curr is in inits so no need to add them
                      accum-lst
                      addr fvars-initss)))))))
                      
(define Fvars-init-withoutadresses
  (lambda (fvars_inits)
  (map cadr fvars_inits)))
                      
(define Fvars-init
	(lambda (addr)
		
		(list 
			    (list addr '* )
			    (list (+ 1 addr) '/ )
				    (list (+ 2 addr) '+ )
				    (list (+ 3 addr) '- )
				    (list (+ 4 addr) 'boolean? )
				    (list (+ 5 addr) 'string-length )
				    (list (+ 6 addr) 'vector-length )
				    (list (+ 7 addr) 'zero? )
				    (list (+ 8 addr) 'string-ref )
				    (list (+ 9 addr) 'pair? )
				    (list (+ 10 addr) 'procedure? )
				    (list (+ 11 addr) 'cons )
				    (list (+ 12 addr) 'car )
				    (list (+ 13 addr) 'cdr )
				    (list (+ 14 addr) 'string? )
				    (list  (+ 15 addr) 'symbol? )
				    (list (+ 16 addr) 'vector? )
				    (list (+ 17 addr) 'make-string )
				    (list (+ 18 addr) 'string-set! )
				    (list (+ 19 addr) 'make-vector )
				    (list (+ 20 addr) 'vector-set! )
				    (list (+ 21 addr) 'vector-ref )
				    (list (+ 22 addr) 'set-cdr! )
				    (list (+ 23 addr) 'set-car! )
				    (list (+ 24 addr) 'char->integer )
				    (list (+ 25 addr) 'integer->char )
				    (list (+ 26 addr) 'remainder )
				    (list (+ 27 addr) 'integer? )
				    (list (+ 28 addr) 'char? )
				    (list (+ 29 addr) 'symbol->string )
				    (list (+ 30 addr) 'eq? )
				    (list (+ 31 addr) 'null? )
				    (list (+ 32 addr) 'number? )
				    (list (+ 33 addr) '= )
				    (list (+ 34 addr) '< )
				    (list (+ 35 addr) '> )
				    (list (+ 36 addr) 'string->symbol )
				    (list (+ 37 addr) 'apply )
				     (list (+ 38 addr) 'vector )
				     (list (+ 39 addr) 'rational? )
				     (list (+ 40 addr) 'numerator )
				     (list (+ 41 addr) 'denominator )

				    )))
      
(define create-fvar-table
  (lambda (pes addr fvars-withOut)
    (fvars->table (organize-fvars (get-fvars pes)) '() addr fvars-withOut) ))
    
    
 ; create list of strings from list of symbols and numbers.     
(define create-string-list-from-list
  (lambda (lst)
    (map (lambda (x)
           (cond ((symbol? x) (symbol->string x))
                 ((number? x) (number->string x))))
         lst)))
         
;;;create a string of the memory image of the constant, given a constant table.
(define const-table->const-string
  (lambda (table)
    (Make-string-of-commas (create-string-list-from-list (apply append (map caddr table))))))
;take list of strings 
; return string sepereted by commas
(define Make-string-of-commas
  (lambda (L_st)
    (fold-left (lambda (expr ls) (string-append expr ", " ls))  `,(car L_st) (cdr L_st))))
;;;create a string of the memory image of the constant, given a constant table.
;;;This is actually the same as the previous procedure and I could've just written (define x [previous procedure])
(define create-cs
  (lambda (table)
      (const-table->const-string table)))
         
(define get-length-of-const-table
    (lambda (const-table)
    (length (create-string-list-from-list (apply append (map caddr const-table))))))
    
(define get-length-of-fvar-table   
  (lambda (table)
    (length table)))
  
  
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CODE_GENARATOR_;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(define get-n-element
  (lambda (elem lst n)
    (cond ((null? lst) #f)
          ((equal? (list-ref (car lst) n) elem) (car lst))
          (else (get-n-element elem (cdr lst) n)))))
  
(define pe-identify
  (lambda (tag)
    (lambda (pe)
 (and (list? pe)  (eq? (car pe) tag) ))))
      
(define parsed-const? (pe-identify 'const))      
(define parsed-define? (pe-identify 'def))
(define parsed-lambda-simple? (pe-identify 'lambda-simple))
(define parsed-lambda-opt? (pe-identify 'lambda-opt))
(define parsed-lambda-variadic? (pe-identify 'lambda-var))
(define parsed-if3? (pe-identify 'if3))
(define parsed-or? (pe-identify 'or))
(define parsed-seq? (pe-identify 'seq)) 
(define parsed-fvar? (pe-identify 'fvar))
(define parsed-pvar? (pe-identify 'pvar))
(define parsed-bvar? (pe-identify 'bvar))
(define parsed-applic? (pe-identify 'applic))
(define parsed-tc-applic? (pe-identify 'tc-applic))
(define parsed-set? (pe-identify 'set))
(define parsed-box-set? (pe-identify 'box-set))
(define parsed-box-get? (pe-identify 'box-get))
(define parsed-box? (pe-identify 'box))

(define code-gen-if3
  (lambda (pe_ size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
  (let ((cgen (lambda (if3 test do-if-true do-if-false)
            (let* ((cgen-test (code-gen test size-env_ num-params_ const-tab_ fvar-tab_ endlabel_))
                  (cgen-dit (code-gen do-if-true size-env_ num-params_ const-tab_ fvar-tab_ endlabel_))
                  (cgen-dif (code-gen do-if-false size-env_ num-params_ const-tab_ fvar-tab_ endlabel_))
                  (lbl-else (label-marker "L_IF3_else_"))
                  (lbl-exit (label-marker "L_IF3_exit_")))
              (string-append
               "  /* IF3 */"
               cgen-test ;;[[test]]
               new-line 
               "  CMP(R0,SOB_BOOLEAN_FALSE);"  
               new-line
               "  JUMP_EQ(" lbl-else ");"
               new-line
               cgen-dit 
               new-line
               "  JUMP(" lbl-exit ");"
               new-line
               lbl-else ":" 
               new-line
               cgen-dif
               new-line
               lbl-exit ":" 
               new-line
               "  /* EXIT IF3 */"
               new-line))))) 
             (apply cgen pe_))))
             

(define code-gen-seq
  (lambda (pe_ size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
 (let ((cgen (lambda (seq pes)
            (apply string-append
                   (map (lambda (pe)
                          (code-gen pe size-env_ num-params_ const-tab_ fvar-tab_ endlabel_))
                        pes))))) 
            (apply cgen pe_))))

(define code-gen-or
  (lambda (pe_ size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
  (let ((cgen (lambda (or pes)
            (let* (
                  (last-pe (get-last-element pes))
                  (all-but-last-pe (remove-last-element pes))
                  (lbl-exit (label-marker "L_or_exit_"))
                  (all-cgen-with-out-last-pe
                   (apply string-append
                          (map (lambda (e)
                                 (string-append
                                  (code-gen e size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
                                  "  CMP(R0,SOB_BOOLEAN_FALSE);"
                                  new-line
                                  "  JUMP_NE(" lbl-exit ");"
                                  new-line))
                               all-but-last-pe)))
                  (last-cgen-pe (code-gen last-pe size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)))
              (string-append
               "  /* OR */" 
               new-line
               all-cgen-with-out-last-pe
               last-cgen-pe
               lbl-exit ":"
               "  /* EXIT OR*/" 
               new-line))))) 
            (apply cgen pe_)) ))                      

   
;(proc (b0 b1 b2 ... bm-1)) proc can't be define or const because proc cant return void 
; proc can be if3 , applic, or,and ,var, let,let*,letrec. ....
(define code-gen-applic
  (lambda (pe_ size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
  
    (let ((cgen (lambda (applic proc args)
            (let* ( 
                  (lbl-proc (label-marker "L_proc_"))
                  (lbl-error (label-marker "L_proc_err_"))
                  (lbl-no-error (label-marker "L_no_proc_err_"))
                  (num-of-args-str (number->string (+ (length args) 1))) )
                  
              (string-append
               "  /* APPLIC */" 
               new-line
               "  PUSH(SOB_NIL); // Reserve space for a potential number of arguments " 
               new-line
               (apply string-append (map
                                     (lambda (arg)
                                     
                                       (string-append
                                        (code-gen arg size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
                                        "  PUSH(R0);" 
                                        new-line))
                                     (reverse args)))
               "PUSH("num-of-args-str");" 
               new-line
               (code-gen proc size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
         
               new-line
               lbl-proc":"
               new-line
               "  PUSH(INDD(R0,1));" new-line ; push the env to the stack
               "  CALLA(INDD(R0,2));" new-line ; call the code 
               ;; because we are not in the tail applic then we want to go back and remove the frame
               "  DROP(1);" new-line ; remove env_
               "  POP(R1);" new-line ; get to r1 the number of args
               "  DROP(R1);" new-line ; remove the args
               "  JUMP("lbl-no-error");" new-line             
               lbl-no-error":"
               " /* END_APPLIC */" new-line
               ))))) (apply cgen pe_))))
  
  
(define (last_element l)
  (cond ((null? (cdr l)) (car l))
        (else (last_element (cdr l)))))


(define code-gen-lambda
  (lambda (lambda-type)
  
    (lambda (pe_ size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
    
      (let* (
	    (parms-of-lambda
		      (cond ((or (eq? lambda-type 'opt) (eq? lambda-type 'simple)) (cadr pe_))
                     ((eq? lambda-type 'variadic) '())))     
            (body (last_element pe_)) ;get last this is body
            (new_env (number->string (+ size-env_ 1)))
            (lbl-cpy-program (label-marker "lbl_cpy_program")) 
            (lbl-code (label-marker "label_code_"))
            (lbl-exit (label-marker "label_exit_"))
            (lbl-copy-loop (label-marker "label_copy_loop_"));; copying the the addresses of the vector 
            (lbl-end-copy-loop (label-marker "label_end_copy_loop_"))
            (lbl-copy-parms-loop (label-marker "label_copy_parms_loop_")) 
            (lbl-end-copy-parms-loop (label-marker "label_end_copy_parms_loop_"))
            (lbl-third-loop (label-marker "lbl_third_loop_")); 
            (lbl-end-third-loop (label-marker "lbl_end_third_loop_")) 
	    (ALLOCATING_MEMORY_FOR_NEW_ENV   (string-append  "  /*lambda-simple*/" new-line
						    "  /* NEW_ENV_ALLOCATING MEMORY */" new-line
						    "  PUSH("new_env");" new-line
						      "  CALL(MALLOC);" new-line
						    "  DROP(1);" new-line
						    "  MOV(R1,R0);" new-line
					    "  /* END_OF_ MEMORY ALLOCATING */" new-line))) ;;result in R1
	    			    
       (string-append
	    ALLOCATING_MEMORY_FOR_NEW_ENV
         "CMP(FP,2);" new-line
         "JUMP_LE("lbl-cpy-program");" new-line
         "MOV(R2,FPARG(0)); /*POINTER TO PRE ENV IN R2 */" new-line
         "MOV(R3, 0); //loop counter" new-line
         "MOV(R4, 1); //index into new env" new-line
      "/* COPYING THE PRE ENV TO NEW ENV ==> R1 POINTS TO NEW ENV , R2 POINTS TO OLD ENV */" new-line
         lbl-copy-loop":" new-line
         "  CMP(R3, "(number->string size-env_)");" new-line ;LOOP CONDITION
         "  JUMP_GE("lbl-end-copy-loop");" new-line
         "  MOV(INDD(R1,R4), INDD(R2,R3));" new-line
         "  ADD(R3, 1);" new-line
         "  ADD(R4, 1);" new-line
         "  JUMP("lbl-copy-loop");" new-line
         lbl-end-copy-loop":" new-line
         "/* COPY OLD ENV TO NEW ENV COMPLETED */" new-line
         
         lbl-cpy-program":" new-line
        "  /*ALLOCATE NEW MEMORY FOR THE NEW ROW R0[0]*/" new-line
         "  PUSH("(number->string num-params_)");" new-line
         "  CALL(MALLOC);" new-line
         "  DROP(1);" new-line
         "  MOV(R3, R0);" new-line
         "  /* END OF ALLOCATING --> ADDR IN R3 */" new-line
 
         "  /* COPY OLD PARMS --> pvars change to bvars*/" new-line
         "  MOV(R5,0); //loop counter" new-line
         lbl-copy-parms-loop":" new-line
         "  CMP(R5,"(number->string num-params_)"); //loop condition" new-line
         "  JUMP_GE("lbl-end-copy-parms-loop");" new-line
         "  MOV(R4,2);" new-line
         "  ADD(R4,R5);" new-line
         "  MOV(INDD(R3,R5),FPARG(R4));" new-line
         "  ADD(R5,1);" new-line
         "  JUMP("lbl-copy-parms-loop");" new-line
         lbl-end-copy-parms-loop":" new-line  
         "  /* DONE COPYING PARMS */" new-line
         "  MOV(INDD(R1,0), R3); 
         /* NOW R1[0] POINTS TO THE FIRST ROW IN THE NEW ENV */" new-line
         
         new-line
         "  /* BUILDING CLOSURE OBJECT AND PUTTING IT IN R0--> THIS IS A IN THE LECTURES and the same for simple and var and opt*/" new-line
         "  PUSH(3);" new-line
         "  CALL(MALLOC);" new-line
         "  DROP(1);" new-line
         "  MOV(INDD(R0,0), T_CLOSURE); /*OBJECT TYPE*/" new-line
         "  MOV(INDD(R0,1), R1); /* POINTER TO THE ENV*/" new-line
         "  MOV(INDD(R0,2), LABEL("lbl-code")); /*POINTER TO THE BODY CODE*/" new-line
         "  /*DONE WITH CREATING CLOSURE OBJECT*/" new-line
         "  JUMP("lbl-exit");" new-line
         new-line
         "  /* BUILDING CODE OF THE BODY OF THE CLOSURE (RUN JUST IF THERE IS APPLY)--> THIS IS B IN THE LECTURES AND CHANGES FROM SIMPLE TO VARIADIC OR OPT LAMBDAS*/" new-line
         lbl-code":" new-line
         "  PUSH(FP);" new-line
         "  MOV(FP,SP);" new-line
         (cond
          ((eq? lambda-type 'simple) 
           (string-append
            "  /* CODE_GEN FOR LAMBDA BODY */" new-line
            (code-gen body (+ size-env_ 1) (length parms-of-lambda) const-tab_ fvar-tab_ endlabel_)
            "  /* END OF CODE GEN*/" new-line))
            
            ;here we have to make stack correction
          (else
           (let ((parms-len (number->string (length parms-of-lambda))))
             (string-append
              "  /* HERE WE MAKE STACK CORRECTION FOR LAMBDA-VAR OR LAMBDA-OPT*/" new-line
              "  MOV(R2, FPARG(1));" new-line ; n -> num of parms
              "  ADD(R2, IMM(1));" new-line ; n+1
              "  MOV(R1, FPARG(R2));" new-line ; last arg
              "  /*MAKE LIST OF OPT PARMS*/" new-line
              "  MOV(R6, FPARG(1));" new-line  ; n -> num of parms
              "  MOV(R7,"parms-len"+1);" new-line ; r7 parm length
              lbl-third-loop":" new-line
              "  CMP(R6,R7);" new-line 
              "  JUMP_LE("lbl-end-third-loop");" new-line
              "  PUSH(R1);" new-line ;  r1=last arg
              "  MOV(R2, FPARG(R6));" new-line ; r2=r6=n -->number of arguments
              "  PUSH(R2);" new-line
              "  CALL(MAKE_SOB_PAIR);" new-line
              "  DROP(2);" new-line 
              "  MOV(R1,R0);" new-line
              "  DECR(R6);" new-line
              "  JUMP("lbl-third-loop");" new-line
              lbl-end-third-loop":" new-line
              "  /*DONE WITH CREATING OPT ARGUMENTS LIST*/" new-line
            ;put opt list arguments after non opt arguments
              "  MOV(R2, SP);" new-line
              "  SUB(R2, IMM(5));" new-line
              "  SUB(R2,IMM("parms-len"));" new-line
              "  MOV(STACK(R2), R1);" new-line
              "  /* cgen for lambda body*/" new-line
              (code-gen body (+ size-env_ 1) (+ 1 (length parms-of-lambda)) const-tab_ fvar-tab_ endlabel_)
             "  /*END OF CODE_GEN AND LAMBDA CORRECTION */" new-line))))
         new-line
         "  POP(FP);" new-line
         "  RETURN;" new-line
         lbl-exit":" new-line)))))

(define code-gen-tc-applic
  (lambda (pe_ size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
    (apply (lambda (tc-applic proc args)
            (let ( 
                  (L-tc-loop (label-marker "L_tc_applic"))
                  (L-tc-exit (label-marker "L_tc_exit"))
                  (argsNum (number->string (+ 1 (length args))))
                  )
                  
                  
                  (string-append 
							"/* TC _ APPLIC */" new-line
							"PUSH(SOB_NIL);" new-line
							(apply string-append (map
							  (lambda (arg)
							      (string-append
								(code-gen arg size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
								  "PUSH(R0);" new-line
									))
							    (reverse args)))
							"PUSH(IMM("(number->string (+ 1 (length args)))"));" ; pushing the number of arguments
							new-line
							(code-gen proc size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
							"PUSH(INDD(R0,1));" ; push env
							new-line
							  "// IN APPLIC TP \n  \n" 
							"  PUSH(FPARG(-1)); " new-line
							"  MOV(R2, FPARG(1)); " new-line
							"  ADD(R2,"argsNum");" new-line
							"  ADD(R2,7);" new-line
							"  MOV(R3,SP);" new-line
							"  SUB(R3,R2);" new-line
							"  MOV(FP,FPARG(-2)); " new-line
							
							"  MOV(R1,FP);"  new-line
							"  MOV(R5,IMM(0));"  new-line
							"  MOV(R6,IMM("argsNum"));" new-line
							"  ADD(R6, IMM(3));" new-line
							
							L-tc-loop ":" new-line
							"  CMP(R5,R6);" new-line
							"  JUMP_GE("L-tc-exit");" new-line
							"  MOV(R7,IMM("argsNum"));" new-line
							"  ADD(R7,IMM(1));" new-line
							"  SUB(R7,R5);" new-line
							"  MOV(STACK(R3), STARG(R7));" new-line
							"  INCR(R3);" new-line
							"  INCR(R5);" new-line
							"  JUMP("L-tc-loop");" new-line
							L-tc-exit ":" new-line
  							"  MOV(SP,R3);" new-line
							"  JUMPA(INDD(R0,2));" new-line
							"/* END TC__applic */"))) pe_)))
          
(define code-gen-pvar
  (lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (apply (lambda (pvar var min_)   
              (string-append        
               "  MOV(R0, FPARG("(number->string (+ min_ 2))"));" new-line             
               )) pe )))

(define code-gen-bvar
  (lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (apply (lambda (bvar var maj min_)
            (string-append
             "  MOV(R0, FPARG(0));" new-line ; env
             "  MOV(R0, INDD(R0,"(number->string maj)"));" new-line ;major
             "  MOV(R0, INDD(R0,"(number->string min_)")); " new-line ; value
            )) pe
          )))
          
(define code-gen-fvar
  (lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (apply (lambda (fvar name)
                  (string-append          
                   "  MOV(R0,IND("(number->string (car (Get-associate-i name fvar-tab 2)))")); " new-line 
                   ))  pe )))     

(define code-gen-define
  (lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (apply 
          (lambda (def var val)
              (string-append
               (code-gen val size-env num-params const-tab fvar-tab endlabel)
             new-line
               "  MOV(IND("(number->string (car (Get-associate-i (cadr var) fvar-tab 2)))"),R0);" new-line ;//set the fvar value in the memory
               "  MOV(R0,SOB_VOID);" new-line ; return void 
              "// code-gen-define End\n"
               )) pe)))
               
           
         
(define code-gen-const 
	(lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (apply
          (lambda (const j)   
              (string-append   
               "  MOV(R0,"(number->string (car (Get-associate-i j const-tab 2)))");" new-line ; calc the add from sym table              
               )) pe)))
               
(define code-gen-set 
	(lambda (pe size-env num-params const-tab fvar-tab endlabel)
	
    (apply 
          (lambda (set var val)
    
          (cond ( (eq? (car var) 'pvar) ;(set (pvar x minor) e)
		      (string-append
			(code-gen val size-env num-params const-tab fvar-tab endlabel)
			new-line
		      "  MOV(FPARG("(number->string (+ (caddr var) 2))"),R0);" new-line 
		      "  MOV(R0,SOB_VOID);" new-line 
		      "// code-gen-set End \n" ) )
		( (eq? (car var) 'bvar) ;(set (bvar x major minor) e)
		
		      (string-append
			(code-gen val size-env num-params const-tab fvar-tab endlabel)
			new-line
			"PUSH(R10);\n"
		          "MOV(R10, FPARG(0));" new-line ; env
			  "MOV(R10, INDD(R10,"(number->string (caddr var))"));" new-line ;major
			  "MOV(INDD(R10,"(number->string (cadddr var))"),R0);" new-line ; set the val
			  "POP(R10); \n"
			  "MOV(R0,SOB_VOID); \n" new-line 
			  
		      "// code-gen-set End \n" ) )
		((eq? (car var) 'fvar) ;(set (fvar x) e)
		      (string-append
			(code-gen val size-env num-params const-tab fvar-tab endlabel)
			new-line
		      "  MOV(IND("(number->string (car (Get-associate-i (cadr var) fvar-tab 2)))"),R0);" new-line ; 
		      "  MOV(R0,SOB_VOID);" new-line 
		      "// code-gen-set end\n")))) pe) ))


(define code-gen-box 
	(lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (apply 
          (lambda (box var) 
              (string-append
             new-line
               "PUSH(IMM(1));" new-line 
               "CALL(MALLOC);" new-line 
               "DROP(1);"new-line
               "MOV(IND(R0),FPARG("(number->string (+ (caddr var) 2))"));"new-line
              "// code-gen-box End\n"
               )) pe)))

(define code-gen-box-set
   (lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (apply 
          (lambda (box-set var val)
              (string-append
             new-line
              (code-gen val size-env num-params const-tab fvar-tab endlabel) new-line
              "MOV(R11,R0);"new-line
              (code-gen var size-env num-params const-tab fvar-tab endlabel) new-line
              "MOV(IND(R0),R11);"new-line
              "MOV(R0,SOB_VOID);" new-line 
              "// code-gen-box-set End\n"
               )) pe)))               

               
(define code-gen-box-get 
	(lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (apply 
          (lambda (box-get var)
              (string-append
             new-line
              (code-gen var size-env num-params const-tab fvar-tab endlabel)new-line
              "MOV(R0,IND(R0));"new-line
              "// code-gen-box-get End\n"
               )) pe)))




(define code-gen
  (lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (let ((params `(,pe ,size-env ,num-params ,const-tab ,fvar-tab ,endlabel)))
      (cond
      ((parsed-const? pe) (apply code-gen-const params))  
       ((parsed-define? pe) (apply code-gen-define params)) 
        ((parsed-lambda-simple? pe) (apply (code-gen-lambda 'simple) params))
        ((parsed-lambda-opt? pe) (apply (code-gen-lambda 'opt) params))
        ((parsed-lambda-variadic? pe) (apply (code-gen-lambda 'variadic) params)) 
       ((parsed-if3? pe) (apply code-gen-if3 params)) 
       ((parsed-or? pe) (apply code-gen-or params)) 
       ((parsed-seq? pe) (apply code-gen-seq params)) 
       ((parsed-fvar? pe) (apply code-gen-fvar params)) 
       ((parsed-pvar? pe) (apply code-gen-pvar params)) 
       ((parsed-bvar? pe) (apply code-gen-bvar params)) 
       ((parsed-applic? pe) (apply code-gen-applic params)) 
       ((parsed-tc-applic? pe) (apply code-gen-tc-applic params)) 
       ((parsed-set? pe) (apply code-gen-set params)) 
       ((parsed-box? pe) (apply code-gen-box params))
       ((parsed-box-set? pe) (apply code-gen-box-set params))
       ((parsed-box-get? pe) (apply code-gen-box-get params)) 
 (else (error 'code-gen "Error: NO EXPRESSION KNOWN"))))))     
 

(define write-to-file
  (lambda (target-file str)
    (let ((fp (open-output-file target-file '(replace))))
      (begin
        (display str fp)
        (close-port fp)))))
        

;get the first symbol address in the constants table
(define First-sym-addr
  (lambda (constan-table)
    (if (null? constan-table)
        -1  
          (if (eq? (list-ref (list-ref (car constan-table) 2) 0) '\T_SYMBOL)
              (caar constan-table)
              (First-sym-addr (cdr constan-table))))))
             
(set! number 0)
(define label-marker
  (lambda (label-name)
        (set! number (+ number 1))
        (string-append label-name
                       (number->string number))))
                       
(define program-end-label (label-marker "L_end_prog")) 
              
(define new-line (list->string (list #\newline)))

(define void-object (if #f #f))

(define make-prologue
  (lambda (constants-table fvars-table frst_sym_address)
    (let ((cont_label_ (label-marker "Cont_label__"))
          (null-addrs (car (Get-associate-i '() constants-table 2)))
          (void_addrs (car (Get-associate-i void-object constants-table 2)))
          (bool-t-addrs (car (Get-associate-i #t constants-table 2)))
          (bool-f-addrs (car (Get-associate-i #f constants-table 2))))
        (string-append
        "#include <stdlib.h>" new-line
        "#include <string.h>" new-line
        "#include <stdio.h>" new-line
        "#include \"arch/cisc.h\"" new-line
         "#include \"arch/debug_macros.h.c\"" new-line
        "#define DO_SHOW 2" new-line
        ; The main function starts here
        "int main()" new-line
        "{" new-line
         "  int i,j;"  new-line
        "  START_MACHINE;" new-line
       "  JUMP(CONTINUE);" new-line
       "#include \"arch/char.lib\"" new-line
       "#include \"arch/io.lib\"" new-line
       "#include \"arch/math.lib\"" new-line
       "#include \"arch/string.lib\"" new-line
       "#include \"arch/system.lib\"" new-line
       "#include \"arch/scheme.lib\"" new-line
       new-line
       "CONTINUE:" new-line
       new-line
       "  #define SOB_VOID "(number->string void_addrs) new-line
       new-line
       "  #define SOB_NIL "(number->string null-addrs) new-line 
       new-line
       "  #define SOB_BOOLEAN_FALSE "(number->string bool-f-addrs) new-line 
       new-line
       "  #define SOB_BOOLEAN_TRUE "(number->string bool-t-addrs) new-line
       new-line
       "JUMP(AFTER_COMPARE);" new-line
       "COMPARE_label:" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	PUSH(R1);" new-line
			"	PUSH(R2);" new-line
			"	PUSH(R3);" new-line
			"	PUSH(R4);" new-line
			"	PUSH(R5);" new-line
			"	MOV(R1,FPARG(0));" new-line
			"	MOV(R2,FPARG(1));" new-line
			"	MOV(R3,INDD(R1,1));" new-line
			"	MOV(R4,INDD(R2,1));" new-line
			"	CMP(IND(R1),T_INTEGER);" new-line
			"	JUMP_EQ(cmp_nums);" new-line
			"	MOV(R1,INDD(R1,2));" new-line
			"	second_check:"	new-line
			" 	CMP(IND(R2),T_INTEGER);" new-line
			"	JUMP_EQ(Two_integers);" new-line
			"	MOV(R2,(INDD(R2,2)));" new-line
			"	General:" new-line
			"	MUL(R3,R2); " new-line
			"	MUL(R1,R4);"	new-line
			"	CMP(R1,R3);" new-line
			"	JUMP_EQ(nums_eq);"	new-line
			"	JUMP_LT(nums_lt);" new-line
			"	MOV(R0,1);"	new-line
			"	JUMP(nums_finish);" new-line
			    "	nums_eq:" new-line
			    "	MOV(R0,0);" new-line
			"	JUMP(nums_finish);" new-line
			"	nums_lt:" new-line
			      "		MOV(R0,-1);" new-line
			"		JUMP(nums_finish);" new-line
			"	cmp_nums:" new-line
			"	MOV(R1,1);" new-line
			"	JUMP(second_check);" new-line
			"	Two_integers:" new-line
			"	MOV(R2,1);" new-line 
			"	JUMP(General);" new-line
			"	nums_finish:" new-line
			"	POP(R5);" new-line
			"	POP(R4);" new-line
			"	POP(R3);" new-line
			"	POP(R2);" new-line
			"	POP(R1);" new-line
			"POP(FP);"new-line
			"RETURN;"new-line
       " AFTER_COMPARE:" new-line
       
((primitive-bigger constants-table fvars-table frst_sym_address)) new-line
((primitive-remainder constants-table fvars-table frst_sym_address))  new-line
((primitive-rational constants-table fvars-table frst_sym_address)) new-line
((primitive-numerator constants-table fvars-table frst_sym_address)) new-line
((primitive-denominator constants-table fvars-table frst_sym_address)) new-line       
((primtive-symbol->string constants-table fvars-table frst_sym_address)) new-line
((prim-cons constants-table fvars-table frst_sym_address)) new-line
((primitive-eq constants-table fvars-table frst_sym_address)) new-line
((primitive-vector constants-table fvars-table frst_sym_address)) new-line
((primitive-make-string constants-table fvars-table frst_sym_address)) new-line
((primitive-make-vector constants-table fvars-table frst_sym_address)) new-line
((primitive-string-set constants-table fvars-table frst_sym_address)) new-line
((primitive-vector-set constants-table fvars-table frst_sym_address)) new-line
((primitive-vector-length constants-table fvars-table frst_sym_address)) new-line
((primitive-vector-ref constants-table fvars-table frst_sym_address)) new-line 
((primitive-str-length constants-table fvars-table frst_sym_address))new-line
((primitive-string-ref constants-table fvars-table frst_sym_address)) new-line
((primtive-set-cdr! constants-table fvars-table frst_sym_address)) new-line
((primtive-set-car! constants-table fvars-table frst_sym_address)) new-line
((primitive-procedure? constants-table fvars-table frst_sym_address)) new-line
((primitive-pair? constants-table fvars-table frst_sym_address)) new-line
((primitive-symbol? constants-table fvars-table frst_sym_address)) new-line
((primitive-string? constants-table fvars-table frst_sym_address)) new-line
((primitive-zero? constants-table fvars-table frst_sym_address)) new-line
((primitive-vector? constants-table fvars-table frst_sym_address)) new-line
((primitive-null? constants-table fvars-table frst_sym_address)) new-line
((primitive-char? constants-table fvars-table frst_sym_address)) new-line
((primitive-integer? constants-table fvars-table frst_sym_address)) new-line
((primitive-boolean? constants-table fvars-table frst_sym_address)) new-line
((primitive-char->integer constants-table fvars-table frst_sym_address)) new-line
((primitive-integer->char constants-table fvars-table frst_sym_address)) new-line
((primitive-car constants-table fvars-table frst_sym_address)) new-line
((primitive-cdr constants-table fvars-table frst_sym_address))  new-line
((prim-string-to-symbol constants-table fvars-table frst_sym_address)) new-line
((primitive-apply constants-table fvars-table frst_sym_address)) new-line
((primitive-symbol-string constants-table fvars-table frst_sym_address)) new-line       
((primitive-number constants-table fvars-table frst_sym_address)) new-line
((primitive-numbers-equal constants-table fvars-table frst_sym_address)) new-line
((primitive-smaller constants-table fvars-table frst_sym_address)) new-line
((primitive-mul constants-table fvars-table frst_sym_address)) new-line 
((primitive-div constants-table fvars-table frst_sym_address)) new-line
((primitive-minus constants-table fvars-table frst_sym_address)) new-line
 ((primitive-plus constants-table fvars-table frst_sym_address)) new-line))))
 
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 
      
 
 
 
(define generate-primitive-closure
    (lambda (lbl addr)   
            (string-append
			"	PUSH(3);" new-line
			"	CALL(MALLOC);" new-line
			"	DROP(1);" new-line
			"	MOV(INDD(R0,0),IMM(T_CLOSURE));" new-line 
			"	MOV(INDD(R0,1),IMM(0));" new-line
			"	MOV(INDD(R0,2),LABEL("lbl"));" new-line
			"	MOV(IND(" (number->string addr) "),R0);" new-line
			
		)))	
		
		
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;new impelmntion
(define primitive-apply
  (lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'apply fvars-table))
		      (lbl-apply (label-marker "Lprim_apply")  )
                      (lbl-closure_apply (label-marker "LPRIM_CLOSURE_apply")  )
                      (lbl-end_apply (label-marker "LPRIM_end_apply")  )
                      )
			(string-append
			"JUMP("lbl-closure_apply");" new-line
			lbl-apply":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP,SP);" new-line
			"MOV(R15,FPARG(-2)); \n"
			"MOV(R4,FP); \n"
			"SUB(R4,IMM(4)) \n; SUB(R4,FPARG(1)); \n"
			"	MOV(R3,FPARG(2));" new-line ;  R3 holds the function 
			"	MOV(R0,FPARG(3)); " new-line ;// R0 holds the list
			"	MOV(R7,IMM(0));" new-line

			"Lprim_apply_list_loop:" new-line
			"	CMP(R0,SOB_NIL);" new-line
			"	JUMP_EQ(Lend_apply_list_loop);" new-line
			"	MOV(R1,INDD(R0,1));" new-line
			"	PUSH(R1);" new-line
			"	MOV(R0,INDD(R0,2));" new-line
			"	ADD(R7,IMM(1));" new-line
			"	JUMP(Lprim_apply_list_loop);" new-line

			"Lend_apply_list_loop:" new-line
                     "       PUSH(SOB_NIL); \n"
			"	MOV(R5,SP);" new-line 
			"	SUB(R5,IMM(1));" new-line
			"MOV(R9,FP);\n"

			"Lreverse_params:" new-line
			"	CMP(R9,R5);" new-line
			"	JUMP_GE(Lapply_in_tp);" new-line
			"	MOV(R6,STACK(R5));" new-line
			"	MOV(STACK(R5),STACK(R9));" new-line
			"	MOV(STACK(R9),R6);" new-line
			"	SUB(R5,IMM(1));" new-line
			"	ADD(R9,IMM(1));" new-line
			"	JUMP(Lreverse_params);" new-line

			"Lapply_in_tp:" new-line
			"PUSH(R7+1);\n"
			"PUSH(INDD(R3,1));\n"
			"PUSH(FPARG(-1));\n"
			"MOV(R5,FP);\n"
		
			"Lapply_in_tp_loop:" new-line
				"	CMP(R5,SP);" new-line
				"	JUMP_EQ(Lend_prim_apply);" new-line
				"	MOV(STACK(R4),STACK(R5));" new-line
				"	ADD(R4,IMM(1));" new-line
				"	ADD(R5,IMM(1));" new-line
				"
				JUMP(Lapply_in_tp_loop);" new-line

			"Lend_prim_apply:" new-line
			"MOV(SP, R4); \n"
			"MOV(FP,R15); \n" 
			"	JUMPA(INDD(R3,2));" new-line
			"POP(FP);\n"
			"RETURN;\n"
			lbl-closure_apply":"  new-line
                        (generate-primitive-closure lbl-apply addr) new-line
			)))))





(define primitive-symbol-string
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'symbol->string fvars-table))
                      (lbl-symbol-string (label-marker "Lprim_symbolstringr")  )
                      (lbl-closure_symbol-string (label-marker "LPRIM_CLOSURE_symbol_string")  )
                      (lbl-end_symbol-string (label-marker "LPRIM_end_symbolstring")  )
                      )
			(string-append
			"JUMP("lbl-closure_symbol-string");" new-line
			lbl-symbol-string":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,1));" new-line
			lbl-end_symbol-string":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
						
			lbl-closure_symbol-string":" new-line
                        (generate-primitive-closure lbl-symbol-string addr) new-line
			)))))

(define primitive-number
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'number? fvars-table))
                      (lbl-number (label-marker "Lprim_number")  )
                      (lbl-closure_number (label-marker "LPRIM_CLOSURE_number")  )
                      (lbl-end_number (label-marker "LPRIM_end_number")  )
                      )
			(string-append
			"JUMP("lbl-closure_number");" new-line
			lbl-number":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			"	MOV(R1,FPARG(2));" new-line
			"	CMP(IND(R1),T_INTEGER);" new-line
			"	JUMP_EQ("lbl-end_number");" new-line
			"	CMP(IND(R1),T_FRACTION);" new-line
			"	JUMP_EQ("lbl-end_number");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
		
			lbl-end_number":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
						
			lbl-closure_number":" new-line
                        (generate-primitive-closure lbl-number addr) new-line
			)))))

(define primitive-remainder
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'remainder fvars-table))
                      (lbl-remainder (label-marker "Lprim_remainder")  )
                      (lbl-closure_remainder (label-marker "LPRIM_CLOSURE_remainder")  )
                      (lbl-end_remainder (label-marker "LPRIM_end_remainder")  )
                      )
			(string-append
			"JUMP("lbl-closure_remainder");" new-line
			lbl-remainder":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R1,FPARG(3));" new-line
			"	MOV(R0,INDD(R0,1)); " new-line
			"	MOV(R1,INDD(R1,1));" new-line
			"	REM(R0,R1);" new-line
			"	PUSH(R0);" new-line
			"	CALL(MAKE_SOB_INTEGER);" new-line
			"	DROP(1);" new-line
			lbl-end_remainder":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
						
			lbl-closure_remainder":" new-line
                        (generate-primitive-closure lbl-remainder addr) new-line
			)))))



(define primitive-rational
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'rational? fvars-table))
                      (lbl-rational (label-marker "Lprim_rational")  )
                      (lbl-closure_rational (label-marker "LPRIM_CLOSURE_rational")  )
                      (lbl-end_rational (label-marker "LPRIM_end_rational")  )
                      )
			(string-append
			"JUMP("lbl-closure_rational");" new-line
			lbl-rational":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			"	MOV(R1,FPARG(2));" new-line
			"	CMP(IND(R1),T_INTEGER);" new-line
			"	JUMP_EQ("lbl-end_rational");" new-line
			"	CMP(IND(R1),T_FRACTION);" new-line
			"	JUMP_EQ("lbl-end_rational");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			lbl-end_rational":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
						
			lbl-closure_rational":" new-line
                        (generate-primitive-closure lbl-rational addr) new-line
			)))))

(define primitive-numerator
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'numerator fvars-table))
                      (lbl-numerator (label-marker "Lprim_numerator")  )
                      (lbl-closure_numerator (label-marker "LPRIM_CLOSURE_numerator")  )
                     
                      )
			(string-append
			"JUMP("lbl-closure_numerator");" new-line
			lbl-numerator":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			
			
			"  	MOV(R0,FPARG(2));" new-line
			" 	MOV(R0,INDD(R0,1));"new-line
			" 	PUSH(R0);" new-line
			" 	CALL(MAKE_SOB_INTEGER);" new-line
			" 	DROP(1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
						
			lbl-closure_numerator":" new-line
                        (generate-primitive-closure lbl-numerator addr) new-line
			)))))



(define primitive-denominator
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'denominator fvars-table))
                      (lbl-denominator (label-marker "Lprim_denominator")  )
                      (IntegerArg (label-marker "L_integer")  )
                      (EndLabel (label-marker "L_end_denom")  )
                      (lbl-closure_denominator (label-marker "LPRIM_CLOSURE_denominator")  )
                     
                      )
			(string-append
			"JUMP("lbl-closure_denominator");" new-line
			lbl-denominator":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"  	MOV(R0,FPARG(2));" new-line
			"CMP(IND(R0), T_INTEGER); \n"
			"JUMP_EQ("IntegerArg"); \n"
			
			" 	MOV(R0,INDD(R0,2));"new-line
			" 	PUSH(R0);" new-line
			" 	CALL(MAKE_SOB_INTEGER);" new-line
			"JUMP("EndLabel"); \n"
			
			IntegerArg":" new-line
			" 	MOV(R0,1);"new-line
			" 	PUSH(R0);" new-line
			" 	CALL(MAKE_SOB_INTEGER);" new-line
			EndLabel":" new-line
			" 	DROP(1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure_denominator":" new-line
                        (generate-primitive-closure lbl-denominator addr) new-line
			)))))
			


(define primitive-mul
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '* fvars-table))
                      (lbl-mul (label-marker "Lprim_mul")  )
                      (lbl-closure_mul (label-marker "LPRIM_CLOSURE_mul")  )
                      (lbl-mul-loop (label-marker "Lprimitive_minus_loop")  )
                      (lbl-mul-intsToFracs (label-marker "Lprimitive_mul_intsToFracs")  )
                      (lbl-mul-AfterUpdate (label-marker "Lprimitive_mul_AfterUpdate")  )
                      (lbl-mul-end (label-marker "Lprimitive_mul_end")  )     
                      )
			(string-append
			"JUMP("lbl-closure_mul");" new-line
			lbl-mul new-line":"
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"PUSH(R1);"new-line
			"PUSH(R2);"new-line
			"PUSH(R3);"new-line
			"PUSH(R4);"new-line
			"PUSH(R5);"new-line
			"PUSH(R6);"new-line
			"PUSH(R7);"new-line
			"  MOV(R1,IMM(1));" new-line
			"    MOV(R2,IMM(1));" new-line
			"    MOV(R5,IMM(0));" new-line	
			 lbl-mul-loop":" new-line
			"	CMP(R5,FPARG(1)-1);" new-line
			"	JUMP_EQ("lbl-mul-end");"new-line
			"	MOV(R6,FPARG(R5+2));"new-line
			"	CMP(IND(R6),T_INTEGER);"new-line
			"	JUMP_EQ("lbl-mul-intsToFracs");"new-line
			"	MOV(R3,INDD(R6,IMM(1)));"new-line
			"	MOV(R4,INDD(R6,IMM(2)));"new-line
			 lbl-mul-AfterUpdate":"  new-line
			"	MUL(R1,R3);"new-line
			"	MUL(R2,R4);"new-line
			"	INCR(R5);"new-line
			"	JUMP("lbl-mul-loop");"new-line
			lbl-mul-intsToFracs":"  new-line
			"	MOV(R3,INDD(R6,1));"new-line
			"	MOV(R4,IMM(1));"new-line
			"	JUMP("lbl-mul-AfterUpdate");"new-line
			lbl-mul-end":" new-line
			"	PUSH(R2);"new-line
			"	PUSH(R1);"new-line
			"	CALL(MAKE_SOB_FRACTION);"new-line
			"	DROP(2);"new-line	
			      "POP(R7);"new-line
			      "POP(R6);"new-line
			      "POP(R5);"new-line
			      "POP(R4);"new-line
			      "POP(R3);"new-line
			      "POP(R2);"new-line
			      "POP(R1);"new-line
			      "	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure_mul":" new-line
                        (generate-primitive-closure lbl-mul addr) new-line
			)))))



(define primitive-div
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '/ fvars-table))
                      (lbl-div (label-marker "Lprim_div")  )
                      (lbl-closure_div (label-marker "LPRIM_CLOSURE_div")  )
                      (lbl-div-loop (label-marker "Lprimitive_div_loop")  )
                      (lbl-div-intsToFracs (label-marker "Lprimitive_div_intsToFracs")  )
                      (lbl-div-AfterUpdate (label-marker "Lprimitive_div_AfterUpdate")  )
                      (lbl-div-One_arg (label-marker "Lprimitive_div_ONe_ARG")  )
                      (lbl-div-end (label-marker "Lprimitive_div_end")  )  
                      (lbl-div-Integer (label-marker "L_Integer")  )
                      )
			(string-append
			"JUMP("lbl-closure_div");" new-line
			lbl-div new-line":"
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R5,FPARG(2));" new-line
			"	MOV(R1,INDD(R5,1));" new-line
			"	MOV(R2,IMM(1));" new-line
			"	CMP(IND(R5),T_INTEGER);" new-line
			"	JUMP_EQ("lbl-div-Integer");" new-line
			"	MOV(R2,INDD(R5,2));" new-line
			lbl-div-Integer":" new-line
			"	CMP(FPARG(1)-1,1);" new-line
			"	JUMP_EQ("lbl-div-One_arg");" new-line
			"	MOV(R5,1);" new-line
			 lbl-div-loop":" new-line
			"	CMP(R5,FPARG(1)-1);" new-line
			"	JUMP_EQ("lbl-div-end");"new-line
			"	MOV(R6,FPARG(R5+2));"new-line
			"	CMP(IND(R6),T_INTEGER);"new-line
			"	JUMP_EQ("lbl-div-intsToFracs");"new-line
			"	MOV(R3,INDD(R6,1));"new-line
			"	MOV(R4,INDD(R6,2));"new-line
			 lbl-div-AfterUpdate":"  new-line
			"	MUL(R1,R4);"new-line
			"	MUL(R2,R3);"new-line
			"	INCR(R5);"new-line
			"	JUMP("lbl-div-loop");"new-line
			lbl-div-intsToFracs":"  new-line
			"	MOV(R3,INDD(R6,1));"new-line
			"	MOV(R4,IMM(1));"new-line
			"	JUMP("lbl-div-AfterUpdate");"new-line

			lbl-div-end":" new-line
			"	PUSH(R2);"new-line
			"	PUSH(R1);"new-line
			"	CALL(MAKE_SOB_FRACTION);"new-line
			"	DROP(2);"new-line		
			"	POP(FP);" new-line
			"	RETURN;" new-line
			
			
			lbl-div-One_arg":"  new-line
			"CMP(IND(R5),T_FRACTION);" new-line
			"JUMP_NE(L_int);" new-line
			"MOV(R1,INDD(R5,2));"
			"MOV(R2,INDD(R5,1));"
			"JUMP("lbl-div-end");" new-line
			"L_int:" new-line
			"MOV(R1,IMM(1));" new-line
			"MOV(R2,INDD(R5,1));" new-line
			"JUMP("lbl-div-end");" new-line

			lbl-closure_div":" new-line
                        (generate-primitive-closure lbl-div addr) new-line
			)))))


(define primitive-minus
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '- fvars-table))
                      (lbl-minus (label-marker "Lprim_minus")  )
                      (lbl-closure_minus (label-marker "LPRIM_CLOSURE_minus")  )
                      (lbl-minus-loop (label-marker "Lprimitive_minus_loop")  )
                      (lbl-minus-intsToFracs (label-marker "Lprimitive_minus_intsToFracs")  )
                      (lbl-minus-AfterUpdate (label-marker "Lprimitive_minus_AfterUpdate")  )
                      (lbl-minus-One_arg (label-marker "Lprimitive_minus_ONe_ARG")  )
                      (lbl-minus-end (label-marker "Lprimitive_minus_end")  )
                      (lbl-minus-Integer (label-marker "L_IntegerR")  )
                      )
		
			(string-append
			"JUMP("lbl-closure_minus");" new-line
			lbl-minus":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R5,FPARG(2));" new-line
			"	MOV(R1,INDD(R5,1));" new-line
			"	MOV(R2,IMM(1));" new-line
			"	CMP(IND(R5),T_INTEGER);" new-line
			"	JUMP_EQ("lbl-minus-Integer");" new-line
			"	MOV(R2,INDD(R5,2));" new-line
			lbl-minus-Integer":" new-line
			"	CMP(FPARG(1)-1,1);" new-line
			"	JUMP_EQ("lbl-minus-One_arg");" new-line
			"	MOV(R5,1);" new-line
			 lbl-minus-loop":" new-line
			"	CMP(R5,FPARG(1)-1);" new-line
			"	JUMP_EQ("lbl-minus-end");"new-line
			"	MOV(R6,FPARG(R5+2));"new-line
			"	CMP(IND(R6),T_INTEGER);"new-line
			"	JUMP_EQ("lbl-minus-intsToFracs");"new-line
			"	MOV(R3,INDD(R6,1));"new-line
			"	MOV(R4,INDD(R6,2));"new-line
			 lbl-minus-AfterUpdate":"  new-line
			"	MUL(R1,R4);"new-line
			"	MUL(R4,R2);"new-line
			"	MUL(R3,R2);"new-line
			"	SUB(R1,R3);"new-line
			"	MOV(R2,R4);"new-line
			"	INCR(R5);"new-line
			
			"	JUMP("lbl-minus-loop");"new-line
			lbl-minus-intsToFracs":"  new-line
			"	MOV(R3,INDD(R6,1));"new-line
			"	MOV(R4,IMM(1));"new-line
			"	JUMP("lbl-minus-AfterUpdate");"new-line

			lbl-minus-end":" new-line
			"	PUSH(R2);"new-line
			"	PUSH(R1);"new-line
			"	CALL(MAKE_SOB_FRACTION);"new-line
			"	DROP(2);"new-line		
			"	POP(FP);" new-line
			"	RETURN;" new-line
			
			
			lbl-minus-One_arg":"  new-line
			"CMP(IND(R5),T_FRACTION);" new-line
			"JUMP_NE(L_inttt);" new-line
			"MUL(R1,-1);" new-line
			"JUMP("lbl-minus-end");" new-line
			"L_inttt:" new-line
			"MUL(R1,-1);" new-line
			"MOV(R2,IMM(1));" new-line
			"JUMP("lbl-minus-end");" new-line
			
			lbl-closure_minus":" new-line
                        (generate-primitive-closure lbl-minus addr) new-line
			)))))

(define primitive-plus
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '+ fvars-table))
                      (lbl-plus (label-marker "Lprim_PLUS")  )
                      (lbl-closure_plus  (label-marker "LPRIM_CLOSURE_PLUS")  )
                      (lbl-plusl-loop (label-marker "Lprimitive_plusl_loop")  )
                      (lbl-plus-intsToFracs (label-marker "Lprimitive_plus_intsToFracs")  )
                      (lbl-plus-AfterUpdate (label-marker "Lprimitive_plus_AfterUpdate")  )
                      (lbl-plus-end (label-marker "Lprimitive_plus_end")  )     
                      )
		
			(string-append
			"JUMP("lbl-closure_plus");" new-line
			lbl-plus":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			
			
			"	MOV(R1,0);" new-line
			"	MOV(R2,1);" new-line
			"	MOV(R5,0);"new-line
			"	MOV(R8,R5);"new-line
			"	ADD(R8,2);"new-line
			 lbl-plusl-loop":" new-line
			"	CMP(R5,FPARG(1)-1);" new-line	
			"	JUMP_EQ("lbl-plus-end");"new-line
			"	MOV(R6,FPARG(R8));"new-line
			"	CMP(IND(R6),T_INTEGER);"new-line
			"	JUMP_EQ("lbl-plus-intsToFracs");"new-line
			"	MOV(R3,INDD(R6,1));"new-line
			"	MOV(R4,INDD(R6,2));"new-line
			 lbl-plus-AfterUpdate":"  new-line
			"	MUL(R1,R4);"new-line
			"	MUL(R4,R2);"new-line
			"	MUL(R3,R2);"new-line
			"	ADD(R1,R3);"new-line
			"	MOV(R2,R4);"new-line
			"	INCR(R5);"new-line
			"	INCR(R8);"new-line
			"	JUMP("lbl-plusl-loop");"new-line
			lbl-plus-intsToFracs":"  new-line
			"	MOV(R3,INDD(R6,1));"new-line
			"	MOV(R4,IMM(1));"new-line
			"	JUMP("lbl-plus-AfterUpdate");"new-line

			lbl-plus-end":" new-line
			"	PUSH(R2);"new-line
			"	PUSH(R1);"new-line
			"	CALL(MAKE_SOB_FRACTION);"new-line
			"	DROP(2);"new-line		
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure_plus":" new-line
                        (generate-primitive-closure lbl-plus addr) new-line
			)))))


(define primitive-numbers-equal
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '= fvars-table))
                      (lbl-numbers-equal (label-marker "Lprim_numbers_equal")  )
                      (lbl-closure_numbers-equal  (label-marker "Lmake_numbers_equal")  )
                      (lbl-numbers-equal-loop (label-marker "Lprimitive_numbersequal_loop")  )
                      (lbl-numbers-equalr-true (label-marker "Lprimitive_numbersequal_true")  )
                      (lbl-numbers-equal-false (label-marker "Lprimitive_numbersequal_false")  )
                      (lbl-numbers-equal-end (label-marker "Lprimitive_numbersequal_end")  )     
                      )
			(string-append
			"JUMP("lbl-closure_numbers-equal");" new-line
			lbl-numbers-equal":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"MOV(R1,FPARG(1)-1); // number of arguments"new-line			 			
			 "MOV(R7,SOB_BOOLEAN_TRUE);"new-line
			 "MOV(R2,IMM(1));" new-line
			 lbl-numbers-equal-loop":" new-line
			 "CMP(R1,R2);"new-line
			 "JUMP_EQ("lbl-numbers-equal-end");" new-line
			 "MOV(R3,FPARG(R2+1));" new-line
			 "MOV(R4,FPARG(R2+2));" new-line
			 "PUSH(R3);" new-line
			 "PUSH(R4);" new-line
			  "CALL(COMPARE_label);"
			"DROP(2);" new-line
			"CMP(R0,0);" new-line
			"JUMP_NE("lbl-numbers-equal-false");" new-line
			"INCR(R2);" new-line
			"JUMP("lbl-numbers-equal-loop");" new-line
			lbl-numbers-equal-false":" new-line
			"	MOV(R7,SOB_BOOLEAN_FALSE);" new-line
			lbl-numbers-equal-end":"
			new-line
			"  MOV(R0,R7);"new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure_numbers-equal":" new-line
                        (generate-primitive-closure lbl-numbers-equal addr) new-line
			)))))


(define primitive-smaller
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '< fvars-table))
                      (lbl-smaller (label-marker "Lprim_smaller")  )
                      (lbl-closure_smaller (label-marker "Lmake_smaller")  )
                      (lbl-smaller-loop (label-marker "Lprimitive_smaller_loop")  )
                      (lbl-smaller-true (label-marker "Lprimitive_smaller_true")  )
                      (lbl-smaller-false (label-marker "Lprimitive_smaller_false")  )
                      (lbl-smaller-end (label-marker "Lprimitive_smaller_end")  )
                      )
			(string-append
			"JUMP("lbl-closure_smaller");" new-line
			lbl-smaller":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			 "MOV(R1,FPARG(1)-1); // number of arguments"new-line			
			 "MOV(R7,SOB_BOOLEAN_TRUE);"new-line
			 "MOV(R2,IMM(1));" new-line
			 lbl-smaller-loop":" new-line
			 "CMP(R1,R2);"new-line
			 "JUMP_EQ("lbl-smaller-end");" new-line
			 "MOV(R3,FPARG(R2+1));" new-line
			 "MOV(R4,FPARG(R2+2));" new-line
			 "PUSH(R3);" new-line
			 "PUSH(R4);" new-line
			  "CALL(COMPARE_label);"
			"DROP(2);" new-line
			"CMP(R0,-1);" new-line
			"JUMP_NE("lbl-smaller-false");" new-line
			"INCR(R2);" new-line
			"JUMP("lbl-smaller-loop");" new-line
			lbl-smaller-false":" new-line
			"	MOV(R7,SOB_BOOLEAN_FALSE);" new-line
			lbl-smaller-end":" new-line
			"  MOV(R0,R7);"new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure_smaller":" new-line
                        (generate-primitive-closure lbl-smaller addr) new-line
			)))))
	
(define primitive-bigger
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '> fvars-table))
                      (lbl-bigger (label-marker "Lprim_bigger")  )
                      (lbl-closure_bigger (label-marker "Lmake_bigger")  )
                      (lbl-bigger-loop (label-marker "Lprimitive_bigger_loop")  )
                      (lbl-bigger-true (label-marker "Lprimitive_bigger_true")  )
                      (lbl-bigger-false (label-marker "Lprimitive_bigger_false")  )
                      (lbl-bigger-end (label-marker "Lprimitive_bigger_end")  ))
			(string-append
			"JUMP("lbl-closure_bigger");" new-line
			lbl-bigger":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			 "MOV(R1,FPARG(1)-1); // number of arguments"new-line			 			
			 "MOV(R7,SOB_BOOLEAN_TRUE);"new-line
			 "MOV(R2,IMM(1));" new-line
			 lbl-bigger-loop":" new-line
			 "CMP(R1,R2);"new-line
			 "JUMP_EQ("lbl-bigger-end");" new-line
			 "MOV(R3,FPARG(R2+1));" new-line
			 "MOV(R4,FPARG(R2+2));" new-line
			 "PUSH(R3);" new-line
			 "PUSH(R4);" new-line
			  "CALL(COMPARE_label);"
			"DROP(2);" new-line
			"CMP(R0,1);" new-line
			"JUMP_NE("lbl-bigger-false");" new-line
			"INCR(R2);" new-line
			"JUMP("lbl-bigger-loop");" new-line
			lbl-bigger-false":" new-line
			"	MOV(R7,SOB_BOOLEAN_FALSE);" new-line
			lbl-bigger-end":" new-line
			"  MOV(R0,R7);"new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure_bigger":" new-line
                        (generate-primitive-closure lbl-bigger addr) new-line
			)))))
	
			
(define primtive-symbol->string
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'symbol->string fvars-table))
                      (lbl-symbol->string (label-marker "Lprim_symbol_string")  )
                      (lbl-closure_symbol->string (label-marker "Lmake_symbol_string")  )
                      )
		(string-append
			"JUMP("lbl-closure_symbol->string");" new-line
			lbl-symbol->string":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,1));" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
                        lbl-closure_symbol->string":" new-line
                        (generate-primitive-closure lbl-symbol->string addr) new-line
			)))))

(define prim-cons
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'cons fvars-table))
                      (lbl-cons (label-marker "Lprim_cons")  )
                      (lbl-closure_cons (label-marker "Lmake_cons")  )
                      )
		(string-append
			"JUMP("lbl-closure_cons");" new-line
			lbl-cons":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	PUSH(FPARG(3));" new-line ;get arg2
			"	PUSH(FPARG(2));" new-line ;get arg1
			"	CALL(MAKE_SOB_PAIR);" new-line
			"	DROP(2);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
                        lbl-closure_cons":" new-line
                        (generate-primitive-closure lbl-cons addr) new-line
			)))))
			

(define primitive-eq
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'eq? fvars-table))
                      (lbl-eq (label-marker "Lprim_eq")  )
                      (lbl-closure-eq (label-marker "Lmake_eq")  )
                      (lbl-eq-cmp-addr (label-marker "Leq_cmp_addr"))
                      (lbl-eq-cmp-val (label-marker "Leq_cmp_val"))
                      (lbl-is-eq (label-marker "Lis_eq"))
                      (lbl-not-eq (label-marker "Lnot_eq"))
                      (Lbl-end-eq (label-marker "Lend_eq")  )
                      (lbl-is-eq-Fraction (label-marker "Leq_fraction")  )
                      (lbl-is-eq-Symbol (label-marker "Leq_sym")  )
                      )
		(string-append
                        "/* Compares two arguments
                         If they are in {T_PAIR, T_STRING, T_VECTOR, T_NIL, T_VOID, T_BOOL} we only compare addresses
                         If they are closures, we compare each of their 3 fields to make sure they are all equal
                         Otherwise they are integers or chars so we compare their values and then their addresses */ " new-line		
			"JUMP("lbl-closure-eq");" new-line
			lbl-eq":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R1,FPARG(2));  // Move the first arg to R0" new-line
			"	MOV(R2,FPARG(3)); // Move the second argument to R1" new-line
			"	MOV(R5,SOB_BOOLEAN_TRUE);" new-line
			"	CMP(INDD(R1,0),INDD(R2,0)); // Compare the types" new-line
			"	JUMP_NE("lbl-not-eq");" new-line
			"	CMP(INDD(R1,0),T_FRACTION);" new-line
			"	JUMP_EQ("lbl-is-eq-Fraction");" new-line
			"	CMP(INDD(R1,0),T_NIL);" new-line
			"	JUMP_EQ("Lbl-end-eq");" new-line
			"	CMP(INDD(R1,0),T_VOID);" new-line
			"	JUMP_EQ("Lbl-end-eq");" new-line
			"	CMP(INDD(R1,0),T_SYMBOL);" new-line
			"	JUMP_EQ("lbl-is-eq-Symbol");" new-line
                        "	CMP(INDD(R1,0),T_STRING);" new-line
			"	JUMP_EQ("lbl-eq-cmp-addr");" new-line
			"	CMP(INDD(R1,0),T_PAIR);" new-line
			"	JUMP_EQ("lbl-eq-cmp-addr");" new-line
			"	CMP(INDD(R1,0),T_VECTOR);" new-line
			"	JUMP_EQ("lbl-eq-cmp-addr");" new-line
			"	CMP(INDD(R1,0),T_BOOL);" new-line
			"	JUMP_EQ("lbl-eq-cmp-addr");" new-line
                        "	CMP(INDD(R1,0),T_CLOSURE); // Checks if both args are closures" new-line
			"	JUMP_NE("lbl-eq-cmp-val");" new-line
                        "       CMP(INDD(R1,0),INDD(R2,0)); // Compares the 3 fields of both closures" new-line
                        "       JUMP_NE("lbl-not-eq"); " new-line
                        "       CMP(INDD(R1,1),INDD(R2,1));" new-line
                        "       JUMP_NE("lbl-not-eq"); " new-line
                        "       CMP(INDD(R1,2),INDD(R2,2));" new-line
                        "       JUMP_NE("lbl-not-eq"); " new-line
			"       JUMP("Lbl-end-eq");" new-line
			lbl-eq-cmp-addr":" new-line
			"	CMP(R1,R2);" new-line
			"	JUMP_EQ("Lbl-end-eq");" new-line
			"	JUMP("lbl-not-eq");" new-line
			lbl-eq-cmp-val":" new-line
			"	CMP(INDD(R2,1),INDD(R1,1));" new-line
			"	JUMP_EQ("Lbl-end-eq");" new-line
			"JUMP("lbl-not-eq");\n"
			lbl-is-eq-Fraction":"  new-line
			"	CMP(INDD(R2,1),INDD(R1,1));" new-line
			"	JUMP_NE("lbl-not-eq");" new-line
			"	CMP(INDD(R2,2),INDD(R1,2));" new-line
			"	JUMP_EQ("Lbl-end-eq");" new-line
			"JUMP("lbl-not-eq");\n"
			lbl-is-eq-Symbol":"  new-line
			"	CMP(INDD(R2,1),INDD(R1,1));" new-line
			"	JUMP_EQ("Lbl-end-eq");" new-line
			lbl-not-eq":" new-line
			"	MOV(R5,SOB_BOOLEAN_FALSE);" new-line
			Lbl-end-eq":" new-line
			"MOV(R0,R5);\n"
			"	POP(FP);" new-line
			"	RETURN;" new-line
                        lbl-closure-eq":" new-line
                        (generate-primitive-closure lbl-eq addr) new-line
			)))))

		
(define primitive-vector
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'vector fvars-table))
                      (lbl-vector (label-marker "Lprim_vector")  )
                      (lbl-closure_vector (label-marker "Lmake_vector")  )
                      (Lbl_vector_loop (label-marker "Lvector_loop")  )
                      (Lbl_vector_end (label-marker "Lvector_end")  )
                      )
		(string-append
			"JUMP("lbl-closure_vector");" new-line
			lbl-vector":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
                        "       MOV(R1, IMM(0))     "
			"	MOV(R2,FPARG(1)-1); " new-line  
			"	MOV(R3,R1); " new-line
			"	ADD(R3,IMM(2)); " new-line
			Lbl_vector_loop":" new-line
			"       CMP(R2 ,R1);" new-line  
			"	JUMP_EQ("Lbl_vector_end");" new-line
			"       PUSH(FPARG(R3)) ;" new-line
			"       ADD(R3, IMM(1)); " new-line
			"       ADD(R1, IMM(1)); " new-line
			"	JUMP("Lbl_vector_loop");" new-line
			Lbl_vector_end":" new-line
			"	PUSH(R2);"	new-line
			"	CALL(MAKE_SOB_VECTOR);" new-line
			"	ADD(R2,IMM(1));" new-line
			"	DROP(R2);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			
			lbl-closure_vector":" new-line
                        (generate-primitive-closure lbl-vector addr) new-line
			
			))))	)			
		
(define primitive-make-string
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'make-string fvars-table))
                      (lbl-make-string (label-marker "Lprim_maketring")  )
                      (lbl-closure-make-string (label-marker "Lmake_make_string")  )
                      (Lbl_make-string_one_param (label-marker "Lmakestring_one_param")  )
                      (Lbl_make-string_loop (label-marker "Lmakestring_loop")  )
                      (Lbl_make-string_end (label-marker "Lmakestring_end")  )
                      )
		(string-append
			"JUMP("lbl-closure-make-string");" new-line
			lbl-make-string":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"MOV(R2,0); \n" 
			"	MOV(R3,IMM(0));" new-line 
			"	MOV(R1,FPARG(2));" new-line ;(len)
			"	MOV(R1,INDD(R1,1));" new-line
			"MOV(R5, FPARG(1)-1); \n"
			"CMP(R5, 1); \n"
			"JUMP_EQ("Lbl_make-string_one_param"); \n"
			"	MOV(R2,FPARG(3)); " new-line ;(value)
			Lbl_make-string_loop":" new-line 
			"	CMP(R3,R1);" new-line ; 
			"	JUMP_EQ("Lbl_make-string_end");" new-line
			"	PUSH(INDD(R2,1));" new-line
			"	ADD(R3,IMM(1));" new-line
			"	JUMP("Lbl_make-string_loop");" new-line
			Lbl_make-string_one_param":" new-line 
			"	CMP(R3,R1);" new-line ; 
			"	JUMP_EQ("Lbl_make-string_end");" new-line
			"	PUSH(R2);" new-line
			"	ADD(R3,IMM(1));" new-line
			"	JUMP("Lbl_make-string_one_param");" new-line
			Lbl_make-string_end":" new-line
			"	PUSH(R1);"	new-line
			"	CALL(MAKE_SOB_STRING);" new-line
			"	ADD(R1,IMM(1));" new-line
			"	DROP(R1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-make-string":" new-line
                        (generate-primitive-closure lbl-make-string addr) new-line
			)))))			
			

			
(define primitive-make-vector
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'make-vector fvars-table))
                      (lbl-make-vector (label-marker "Lprim_make_vector")  )
                      (lbl-closure-make-vector (label-marker "Lmake_make_vector")  )
                      
                      (Lbl_vec_loop (label-marker "Lvec_loop")  )
                      (Lbl_vec_end (label-marker "Lvec_end")  )
                      )
		(string-append
			"JUMP("lbl-closure-make-vector");" new-line
			lbl-make-vector":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"PUSH(2);\n"
			"CALL(MALLOC);\n"
			"DROP(1);\n"
			"MOV(IND(R0),T_INTEGER); \n"
			"MOV(INDD(R0,1),IMM(0)); \n"
			"MOV(R2,R0); \n" 
			"	MOV(R3,IMM(0));" new-line 
			"	MOV(R1,FPARG(2));" new-line ;(len)
			"	MOV(R1,INDD(R1,1));" new-line
			"MOV(R5, FPARG(1)-1); \n"
			"CMP(R5, 1); \n"
			"JUMP_EQ("Lbl_vec_loop"); \n"
			"	MOV(R2,FPARG(3)); " new-line ;(value)
			Lbl_vec_loop":" new-line 
			"	CMP(R3,R1);" new-line ; 
			"	JUMP_EQ("Lbl_vec_end");" new-line
			"	PUSH(R2);" new-line
			"	ADD(R3,IMM(1));" new-line
			"	JUMP("Lbl_vec_loop");" new-line
			Lbl_vec_end":" new-line
			"	PUSH(R1);"	new-line
			"	CALL(MAKE_SOB_VECTOR);" new-line
			"	ADD(R1,IMM(1));" new-line
			"	DROP(R1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-make-vector":" new-line
                        (generate-primitive-closure lbl-make-vector addr) new-line
			)))))		
			
 
(define primitive-string-set
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'string-set! fvars-table))
                      (lbl-string-set (label-marker "Lprim_string_set")  )
                      (lbl-closure-string-set (label-marker "Lmake_string_set")  ))
		(string-append
			"JUMP("lbl-closure-string-set");" new-line
			lbl-string-set":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line  ; The string param
			"	MOV(R1,FPARG(3));" new-line  ; index param
			"	MOV(R2,FPARG(4));" new-line  ; char param
			"	MOV(R2,INDD(R2,1));" new-line
			"	MOV(R1,INDD(R1,1));" new-line
			"	ADD(R1,IMM(2));" new-line
			"	MOV(INDD(R0,R1),R2);" new-line  ; Moving the char to the given index
			"	MOV(R0,SOB_VOID);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-string-set":" new-line
                        (generate-primitive-closure lbl-string-set addr) new-line
	)))))
	
(define primitive-vector-set
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'vector-set! fvars-table))
                      (lbl-vector-set (label-marker "Lprim_vector_set")  )
                      (lbl-closure-vector-set (label-marker "Lmake_vector_set")  ))
		(string-append
			"JUMP("lbl-closure-vector-set");" new-line
			lbl-vector-set":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line  ; The vector param
			"	MOV(R1,FPARG(3));" new-line  ; index param
			"	MOV(R2,FPARG(4));" new-line  ; obj param
			"	MOV(R1,INDD(R1,1));" new-line
			"	ADD(R1,IMM(2));" new-line
			"	MOV(INDD(R0,R1),R2);" new-line  ; Moving the obj to the given index
			"	MOV(R0,SOB_VOID);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-vector-set":" new-line
                        (generate-primitive-closure lbl-vector-set addr) new-line
	)))))
			
(define primitive-vector-length
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'vector-length fvars-table))
                      (lbl-vector-length (label-marker "Lprim_vector_length")  )
                      (lbl-closure-vector-length (label-marker "Lmake_vector_length")  ))
		(string-append
			"JUMP("lbl-closure-vector-length");" new-line
			lbl-vector-length":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,1));" new-line
			"	PUSH(R0);" new-line
			"	CALL(MAKE_SOB_INTEGER);" new-line
			"	DROP(1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
                        lbl-closure-vector-length":" new-line
                        (generate-primitive-closure lbl-vector-length addr) new-line

			)))))		
			
(define primitive-vector-ref
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'vector-ref fvars-table))
                      (lbl-vector-ref (label-marker "Lprim_vector_ref")  )
                      (lbl-closure-vector-ref (label-marker "Lmake_vector_ref")  ))
		(string-append
			"JUMP("lbl-closure-vector-ref");" new-line
			lbl-vector-ref":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R1,FPARG(2));" new-line ; vector arg
			"	MOV(R2,FPARG(3));" new-line      ; Int object
			"	MOV(R5,INDD(R2,1));" new-line ; int-ref to the relevant index 
			"	ADD(R5,IMM(2));" new-line
			"	MOV(R0,INDD(R1,R5));" new-line ; get the value in the index
			"	POP(FP);" new-line
			"	RETURN;" new-line

			lbl-closure-vector-ref":" new-line
                        (generate-primitive-closure lbl-vector-ref addr) new-line
			)))))
			
			
			
			
(define primitive-str-length
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'string-length fvars-table))
                      (lbl-str-length (label-marker "Lprim_str_length")  )
                      (lbl-closure-str-length (label-marker "Lmake_str_length")  ))
		(string-append
			"JUMP("lbl-closure-str-length");" new-line
			lbl-str-length":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,1));" new-line
			"	PUSH(R0);" new-line
			"	CALL(MAKE_SOB_INTEGER);" new-line
			"	DROP(1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
                        lbl-closure-str-length":" new-line
                        (generate-primitive-closure lbl-str-length addr) new-line
                        )))))
			

(define primitive-string-ref
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'string-ref fvars-table))
                      (lbl-string-ref (label-marker "Lprim_string_ref")  )
                      (lbl-closure-string-ref (label-marker "Lmake_string_ref")  ))
		(string-append
			"JUMP("lbl-closure-string-ref");" new-line
			lbl-string-ref":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line ; string arg
			"	MOV(R1,FPARG(3));" new-line      ; Int object
			"	MOV(R1,INDD(R1,1));" new-line ; int-ref to the relevant index 
			"	ADD(R1,IMM(2));" new-line
			"	MOV(R0,INDD(R0,R1));" new-line ; get the value in the index
			"	PUSH(R0);" new-line
			"	CALL(MAKE_SOB_CHAR);" new-line
			"	DROP(1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-string-ref":" new-line
                        (generate-primitive-closure lbl-string-ref addr) new-line
			))))	)			
			

		
    
			
			
			
(define primtive-set-cdr!
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'set-cdr! fvars-table))
                      (lbl-set-cdr! (label-marker "Lprim_setcdr")  )
                      (lbl-closure-set-cdr! (label-marker "Lmake_set_cdr")  ))
		(string-append
			"JUMP("lbl-closure-set-cdr!");" new-line
			lbl-set-cdr!":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R1,FPARG(2));" new-line ;pair
			"	MOV(R2,FPARG(3));" new-line ;obj
			"	MOV(INDD(R1,2),R2);" new-line
			"	MOV(R0,SOB_VOID);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-set-cdr!":" new-line
                        (generate-primitive-closure lbl-set-cdr! addr) new-line
			
			)))))
			
(define primtive-set-car!
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'set-car! fvars-table))
                      (lbl-set-car! (label-marker "Lprim_set_car")  )
                      (lbl-closure-set-car!(label-marker "Lmake_set_car")  ))
		(string-append
			"JUMP("lbl-closure-set-car!");" new-line
			lbl-set-car!":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R1,FPARG(2));" new-line ;pair
			"	MOV(R2,FPARG(3));" new-line ;obj
			"	MOV(INDD(R1,1),R2);" new-line
			"	MOV(R0,SOB_VOID);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-set-car!":" new-line
                        (generate-primitive-closure lbl-set-car! addr) new-line
			
			)))))
			
(define primitive-procedure?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'procedure? fvars-table))
                      (lbl-procedure? (label-marker "Lprim_procedure")  )
                      (lbl-closure-procedure? (label-marker "Lmake_procedure")  )
                     (lbl-is-procedure (label-marker "L_is_procedure")  )
                     (lbl-end-procedure (label-marker "L_end_procedure")  ))
		(string-append
			"JUMP("lbl-closure-procedure?");" new-line
			lbl-procedure?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_CLOSURE);" new-line
			"	JUMP_EQ("lbl-is-procedure");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-procedure");" new-line
			lbl-is-procedure":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-procedure":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-procedure?":" new-line
                (generate-primitive-closure lbl-procedure? addr) new-line
		)))))
			
			
(define primitive-pair?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'pair? fvars-table))
                      (lbl-pair? (label-marker "Lprim_pair")  )
                      (lbl-closure-pair? (label-marker "Lmake_pair")  )
                     (lbl-is-pair (label-marker "L_is_pair")  )
                     (lbl-end-pair (label-marker "L_end_pair")  ))
		(string-append
			"JUMP("lbl-closure-pair?");" new-line
			lbl-pair?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_PAIR);" new-line
			"	JUMP_EQ("lbl-is-pair");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-pair");" new-line
			lbl-is-pair":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-pair":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-pair?":" new-line
                (generate-primitive-closure lbl-pair? addr) new-line
		)))))
			
(define primitive-symbol?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'symbol? fvars-table))
                      (lbl-symbol? (label-marker "Lprim_symbol")  )
                      (lbl-closure-symbol? (label-marker "Lmake_symbol")  )
                     (lbl-is-symbol (label-marker "L_is_symbol")  )
                     (lbl-end-symbol (label-marker "L_end_symbol")  ))
		(string-append
			"JUMP("lbl-closure-symbol?");" new-line
			lbl-symbol?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_SYMBOL);" new-line
			"	JUMP_EQ("lbl-is-symbol");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-symbol");" new-line
			lbl-is-symbol":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-symbol":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-symbol?":" new-line
                (generate-primitive-closure lbl-symbol? addr) new-line
		)))))
;; 		
(define primitive-string?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'string? fvars-table))
                      (lbl-string? (label-marker "Lprim_string")  )
                      (lbl-closure-string? (label-marker "Lmake_string")  )
                     (lbl-is-string (label-marker "L_is_string")  )
                     (lbl-end-string (label-marker "L_end_string")  ))
		(string-append
			"JUMP("lbl-closure-string?");" new-line
			lbl-string?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_STRING);" new-line
			"	JUMP_EQ("lbl-is-string");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-string");" new-line
			lbl-is-string":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-string":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-string?":" new-line
                (generate-primitive-closure lbl-string? addr) new-line
		)))))
		
		
(define primitive-zero?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'zero? fvars-table))
                      (lbl-zero? (label-marker "Lprim_zero")  )
                      (lbl-closure-zero? (label-marker "Lmake_zero")  )
                     (lbl-is-zero? (label-marker "L_is_zero")  )
                     (lbl-end-zero (label-marker "L_end_zero")  ))
		(string-append
			"JUMP("lbl-closure-zero?");" new-line
			lbl-zero?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,1));" new-line
			"	CMP(R0,IMM(0));" new-line
			"	JUMP_EQ("lbl-is-zero?");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-zero");" new-line
			lbl-is-zero?":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-zero":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-zero?":" new-line
                (generate-primitive-closure lbl-zero? addr) new-line
		)))))
		
(define primitive-vector?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'vector? fvars-table))
                      (lbl-vector? (label-marker "Lprim_vector")  )
                      (lbl-closure-vector? (label-marker "Lmake_vector")  )
                     (lbl-is-vector (label-marker "L_is_vector")  )
                     (lbl-end-vector (label-marker "L_end_vector")  ))
		(string-append
			"JUMP("lbl-closure-vector?");" new-line
			lbl-vector?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_VECTOR);" new-line
			"	JUMP_EQ("lbl-is-vector");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-vector");" new-line
			lbl-is-vector":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-vector":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-vector?":" new-line
                (generate-primitive-closure lbl-vector? addr) new-line
		)))))
		

(define primitive-null?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'null? fvars-table))
                      (lbl-null? (label-marker "Lprim_null")  )
                      (lbl-closure-null? (label-marker "Lmake_null")  )
                     (lbl-is-null (label-marker "L_is_null")  )
                     (lbl-end-null (label-marker "L_end_null")  ))
		(string-append
			"JUMP("lbl-closure-null?");" new-line
			lbl-null?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_NIL);" new-line
			"	JUMP_EQ("lbl-is-null");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-null");" new-line
			lbl-is-null":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-null":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-null?":" new-line
                (generate-primitive-closure lbl-null? addr) new-line
		)))))
			
			
(define primitive-char?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'char? fvars-table))
                      (lbl-char? (label-marker "Lprim_char")  )
                      (lbl-closure-char? (label-marker "Lmake_char")  )
                     (lbl-is-char (label-marker "L_is_char")  )
                     (lbl-end-char (label-marker "L_end_char")  ))
		(string-append
			"JUMP("lbl-closure-char?");" new-line
			lbl-char?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_CHAR);" new-line
			"	JUMP_EQ("lbl-is-char");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-char");" new-line
			lbl-is-char":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-char":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-char?":" new-line
                (generate-primitive-closure lbl-char? addr) new-line
		)))) 	)		

(define primitive-integer?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'integer? fvars-table))
                      (lbl-integer? (label-marker "Lprim_integer")  )
                      (lbl-closure-integer? (label-marker "Lmake_integer")  )
                     (lbl-is-integer (label-marker "L_is_integer")  )
                     (lbl-end-integer (label-marker "L_end_integer")  ))
		(string-append
			"JUMP("lbl-closure-integer?");" new-line
			lbl-integer?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_INTEGER);" new-line
			"	JUMP_EQ("lbl-is-integer");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-integer");" new-line
			lbl-is-integer":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-integer":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-integer?":" new-line
                (generate-primitive-closure lbl-integer? addr) new-line
		)))))
		
(define primitive-boolean?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'boolean? fvars-table))
                      (lbl-boolean? (label-marker "Lprim_boolean")  )
                      (lbl-closure-boolean? (label-marker "Lmake_boolean")  )
                     (lbl-is-boolean (label-marker "L_is_boolean")  )
                     (lbl-end-boolean (label-marker "L_end_boolean")  ))
		(string-append
			"JUMP("lbl-closure-boolean?");" new-line
			lbl-boolean?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
		
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_BOOL);" new-line
			"	JUMP_EQ("lbl-is-boolean");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-boolean");" new-line
			lbl-is-boolean":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-boolean":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-boolean?":" new-line
                (generate-primitive-closure lbl-boolean? addr) new-line
		)))))
			
			
			
(define primitive-char->integer
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'char->integer fvars-table))
                      (lbl-char->integer (label-marker "Lprim_char_integer")  )
                      (lbl-closure-char->integer (label-marker "Lmake_char_integer")  ))
		(string-append
			"JUMP("lbl-closure-char->integer");" new-line
			lbl-char->integer":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2)); // char" new-line ; param char
			"	MOV(R0,INDD(R0,1));" new-line ;char value
			"	PUSH(R0);" new-line 
			"	CALL(MAKE_SOB_INTEGER);" new-line 
			"	DROP(1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-char->integer":" new-line
                (generate-primitive-closure lbl-char->integer addr) new-line
		
		)))))
			
(define primitive-integer->char
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'integer->char fvars-table))
                      (lbl-integer->char (label-marker "Lprim_integer_char")  )
                      (lbl-closure-integer->char (label-marker "Lmake_integer_char")  ))
		(string-append
			"JUMP("lbl-closure-integer->char");" new-line
			lbl-integer->char":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2)); // char" new-line ; param char
			"	MOV(R0,INDD(R0,1));" new-line ;char value
			"	PUSH(R0);" new-line 
			"	CALL(MAKE_SOB_CHAR);" new-line 
			"	DROP(1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-integer->char":" new-line
                (generate-primitive-closure lbl-integer->char addr) new-line
		
		)))))
		
(define primitive-car
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'car fvars-table))
                      (lbl-car (label-marker "Lprim_car")  )
                      (lbl-closure-car (label-marker "Lmake_primitive_car")  ))
                      
		(string-append
			"JUMP("lbl-closure-car");" new-line
			lbl-car":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line 
			"	MOV(R0,INDD(R0,1));" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-car":" new-line
			(generate-primitive-closure lbl-car addr) new-line
			)))))
			
(define primitive-cdr
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'cdr fvars-table))
                      (lbl-cdr (label-marker "Lprim_cdr")  )
                      (lbl-closure-cdr (label-marker "Lmake_primitive_cdr")  ))
                      
		(string-append
			"JUMP("lbl-closure-cdr");" new-line
			lbl-cdr":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line 
			"	MOV(R0,INDD(R0,2));" new-line 
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-cdr":" new-line
			(generate-primitive-closure lbl-cdr addr) new-line
			)))))





(define prim-string-to-symbol
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'string->symbol fvars-table))
                      (lbl-str->sym (label-marker "Lprim_str_sym")  )
                      (lbl-closure-str->sym (label-marker "Lmake_prim_str_sym")  )
                      (lbl-done (label-marker "L_str_sym_done")  )
                      (lbl-loop (label-marker "L_str_sym_loop"))
                      (lbl-new-sym (label-marker "L_str_sym_new_sym"))
                      )
                      
		(string-append
			"JUMP("lbl-closure-str->sym");" new-line
			lbl-str->sym":" new-line
			"  PUSH(FP);" new-line
                        "  MOV(FP,SP);" new-line
                        "  PUSH(R11);" new-line
                        "  PUSH(R12);" new-line
                        "  PUSH(R13);" new-line
                        "  PUSH(R14);" new-line
                        "  MOV(R12, FPARG(2));" new-line ; our param --> string
                        "  MOV(R11,"(number->string frst_sym_address)");" new-line; first real symbol
                        lbl-loop":" new-line
                        "  CMP(R11,IMM(-1)); " new-line ; to know that we are not at the end of the symbol table
                        "  JUMP_EQ("lbl-new-sym"); " new-line
                        "  MOV(R13,INDD(R11,1)); " new-line ; if the curr symbol exists -> point r13 to the string address
                        "  MOV(R0,R11); " new-line ; r11 = r0 = curr symbol {to return it in case we have match}
			  ; comapring the strings  
			"      
			MOV(R15,SOB_BOOLEAN_FALSE); \n
			CMP(INDD(R13,1),INDD(R12,1)); \n
			JUMP_NE(L_compare_strings_finish); \n
			MOV(R3,INDD(R13,1)); \n
			ADD(R3,2); \n
			MOV(R4,2); \n
			L_compare_strings_loop: \n
			CMP(R4,R3);  \n
			JUMP_EQ(L_compare_strings_true); \n
			CMP(INDD(R13,R4),INDD(R12,R4));  \n
			JUMP_NE(L_compare_strings_finish); \n
			INCR(R4); \n
			JUMP(L_compare_strings_loop); \n
			L_compare_strings_true: \n
			MOV(R15,SOB_BOOLEAN_TRUE); \n 
			L_compare_strings_finish: \n"
		"CMP(R15,SOB_BOOLEAN_TRUE);\n"
                        "  JUMP_EQ("lbl-done"); " new-line
                        "  MOV(R14, R11); " new-line
                        "  MOV(R11, INDD(R11,2)); " new-line
                        "  JUMP("lbl-loop");" new-line
       
                        lbl-new-sym":" new-line
                        "  PUSH(IMM(3));" new-line ;allocating for new sym
                        "  CALL(MALLOC);" new-line
                        "  DROP(1);" new-line
                        "  MOV(INDD(R14,2),IMM(R0));" new-line ; update the prev pointer to point to the  new symbol
                        "  MOV(IND(R0), T_SYMBOL);" new-line ;type
                        "  MOV(INDD(R0,1), IMM(R12));" new-line ;pointer to the string
                        "  MOV(INDD(R0,2), IMM(-1));" new-line ;point to the next -1
                                                     
                        lbl-done":" 
                        new-line
                        "  POP(R14);" new-line
                        "  POP(R13);" new-line
                        "  POP(R12);" new-line
                        "  POP(R11);" new-line
                        "  POP(FP);" new-line
                        "  RETURN;" new-line

			lbl-closure-str->sym":" new-line
			(generate-primitive-closure lbl-str->sym addr) new-line
			)))))
;;;;;;;;;;;;;;;;;;;;;;;;




  
(define initiate-memory
    (lambda (const-tab_ fvar-tab_)
    (let* (
           (num-of-constansts (get-length-of-const-table const-tab_))
           (first-address (caar const-tab_))
           (last-address (+ first-address num-of-constansts (get-length-of-fvar-table fvar-tab_) 1)))
      (string-append
      new-line
       "  long mem["(number->string num-of-constansts)"] = {"(create-cs const-tab_)"}; " new-line
       "  memcpy(&ADDR("(number->string first-address)"), mem, sizeof(mem)); " new-line
         
       "MOV(IND(0),"(number->string last-address)");" new-line
       "  /* end of initialization */" new-line
       ))))
  
  

  (define epilogue
  (string-append
   "  /* Stopping the program...   */ " new-line
    program-end-label ":" 
   new-line
     "STOP_MACHINE; "
   new-line
   "  return 0;" 
   new-line
   "} " 
   new-line))

   
(define gen-epilogue-sexpr
  (lambda (lbl) 
  (let ((L_epilogueSexpr (label-marker lbl))
	(DOne_label (label-marker "Done__lbl"))
  )
      (string-append
       L_epilogueSexpr":"
       "  /*	print the content of R0 */" new-line
       "  CMP(R0, SOB_VOID);" new-line
       "  JUMP_EQ("DOne_label");" new-line
       "  PUSH(R0);" new-line
       "  CALL(WRITE_SOB);" new-line 
       "  CALL(NEWLINE);" new-line
       "  DROP(1);" new-line
       DOne_label":" new-line
       ))))
 
(define compile-scheme-file      
  (lambda (src target)
  (let* ( (sexprs  (test-s <sexpr> (file->string src)))
	    (with-support-sexprs (test-s <sexpr> (file->string "SchemePrims.scm")))
	    (pe-sexprs-list (map (lambda (sexpr) (full-parsed sexpr)) sexprs ))	    
	    (pe-With-support (map (lambda (expr)
                          (full-parsed expr))
                        with-support-sexprs))                       
             (pe-lst (append pe-With-support pe-sexprs-list)) 
            ; (pe-lst pe-sexprs-list)
             (constants-table (create-const-table pe-lst 300))
             (constants-table-length (get-length-of-const-table constants-table))
             (init_fvars (Fvars-init (+ 300 constants-table-length)))
             (inits-fvars-withOut_address (Fvars-init-withoutadresses init_fvars))
              (init_fvars_length (length init_fvars))
              (fvars-table (create-fvar-table pe-lst (+ 300 constants-table-length init_fvars_length) inits-fvars-withOut_address))
              (fvars-table (append init_fvars fvars-table))
            (memory-init (initiate-memory constants-table fvars-table))
              (addr-of-first-sym (First-sym-addr (reverse constants-table)))
              (prologue (make-prologue constants-table fvars-table addr-of-first-sym))
              (last-address (+ 300 constants-table-length init_fvars_length 1))
              (outCode 
              (apply string-append (map
                                    (lambda (x)
                                        (string-append
                                         (code-gen x 0 0 constants-table fvars-table "L_epilogueSexpr")
                                         (gen-epilogue-sexpr "L_epilogueSexpr")))
                                    pe-lst)))
              (final-code (string-append prologue memory-init outCode epilogue ))
	    ) 
        (write-to-file target final-code))))




	    

	  
