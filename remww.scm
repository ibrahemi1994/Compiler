(define read_elements
    (lambda (instruction)
      (cadr instruction)))
(define write_elements
    (lambda (instruction)
      (caddr instruction)))
(define (Notmember? x listt)
     (if (null? listt) #t                                
         (if (equal? x (car listt)) #f                  
              (Notmember? x (cdr listt))))) 
        
;; overright the map by mymap
(define (mymap f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (mymap f (cdr lst)))))
             

    
(define rec_help
(lambda (element rest_instructions) 
(if (null? rest_instructions) #f			 
				(if (not (Notmember? element (read_elements (car rest_instructions)))) ;element exists in reads
							#f 
				      ;else
					(if (not (Notmember? element (write_elements (car rest_instructions))))
						    #t
						    ;else
						   ( rec_help element (cdr rest_instructions))
						 
				      )
				) 
				)))
    
             
(define to_remove_instruction? 
  (lambda
    (instruction rest_instructions)
  ; we have 2 somthings to deal with...
  ;for every element in right side in the instruction .. we check in the rest body (instructions) for every inst - if (element exists in left side) return false else--> if( elemnt exists in right side return true) else contunio next inst
  
  
     (andmap (lambda (element ) (rec_help element rest_instructions))
				
				(write_elements instruction) )
  
    ))


(define remww_h
  (lambda (assembly_instructions)
  (if (null? assembly_instructions ) assembly_instructions
	      (if (to_remove_instruction? (car assembly_instructions) (cdr assembly_instructions) )
		  (remww_h (cdr assembly_instructions))
		  (append (list (car assembly_instructions)) (remww_h (cdr assembly_instructions)))  
		  ))
      )) 


(define remww ; fixed point
  (lambda (assembly_instructions)
    (if (equal? assembly_instructions (remww_h assembly_instructions))
	      assembly_instructions
	      (remww (remww_h assembly_instructions)))))      
      