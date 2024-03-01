(define data (list 1 2 3 4 5 6 7 8 9 0))

(let loop ((l data))
 (if (null? l) 'quit
	(begin 
	  (format #t "\t~a\n" (car l))
	  (loop (cdr l))
	)
 )
)

(define Reverse 
 (lambda (l)
  (let loop ((l l) (res '()))
    (if (null? l) res
      (loop (cdr l) (cons (car l) res))
    )
  )
 )    
)

(format #t "Reverse: ~a\n" (Reverse data))

(list-ref l index)
(list-set! l index value)


