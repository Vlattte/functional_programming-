(define Tree '(
							 ( 
								(() 4 ()) 
								2 
								(() 5 ()) 
							 ) 
							 1 
							 ( 
								(() 6 ()) 
								3 
								(() 7 ())
							 )
							)
)

(define Tree2 '(
							 ( 
								(() 1 ()) 
								2 
								(() 1 ()) 
							 ) 
							 1 
							 ( 
								() 
								3 
								(() 7 ())
							 )
							)
)
(define left-branch car)
(define right-branch caddr)
(define node-value cadr)	

(define (make-tree-iterator tree)
 (let ((caller #f))
  (letrec ((traverse  
						(lambda ()
						 (let loop ((tree tree))
							(if (not (null? tree))
							 (begin
								(loop (left-branch tree))
								
								(call/cc (lambda (rest-of-tree) 
													(set! traverse (lambda () (rest-of-tree 'dummy)))
													(caller (node-value tree))
												 )
								)
								(loop (right-branch tree))
							 )
							)
						 )
						 (caller 'end)
							
						)
					)) 

	 (lambda () 
		(call/cc (lambda (k) (set! caller k) (traverse)))
	 ) 
	)
 )
)


(define (compare-tree t1 t2)
(let ((_yield #f) (iter1 (make-tree-iterator t1)) (iter2 (make-tree-iterator t2)) )
	(letrec ((get-next-proc
				(lambda ()
					(let loop ((v1 iter1) (v2 iter2))					
						(begin
							(cond 				
								((eq? v1 'end) (_yield (eq? v2 'end)) )
								((eq? v2 'end) (_yield #f) )
								((eq? v1 v2) (loop (iter1) (iter2)))
								(else (_yield #f))
							)
						)	
					)
				)
			))
		(call/cc (lambda (caller)
					(set! _yield caller)
					(get-next-proc)
				 )
		)
    )
 )
)

(format #t "\n\tCompare result: ~a\n" (compare-tree Tree Tree2))

