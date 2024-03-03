(define data (list 1 0 7 2 3 4 8 9 5 6))

(define quicksort       
    (lambda (l)
        (let sort ((l l))                           
            (begin 
                (define pivot 0)        ; опорный элемент
                (define low '() )       ; меньше опорного элемента
                (define high '() )      ; больше опорного элемента
                
                ; тут делим по массивам low и high 
                (if (null? (cdr l)) 
                    (begin
                        (format #t "EMPTY\n")
                        l
                    )  
                    (begin             
                        (set! pivot (car l))
                        (let sort_loop ((l l) (pivot pivot) (low low) (high high))
                            (if (null? l)       ; если больше нет элементов, то все
                                (begin 
                                    (format #t "aaaaaaaa = ~a\n" pivot)

                                    ; если low не пустой (в high всегда хотя бы сам элемент лежит)
                                    (if (not (null? low) )
                                        (set! low (sort low))
                                    )                                    
                                    (set! high (sort high))
                                    (cons low high)
                                )
                                (begin         
                                    (format #t "pivot = ~a\n" pivot)               
                                    (if (> pivot (car l)) 
                                        (begin 
                                            (set! low (cons (car l) low ))
                                            (format #t "low = ~a\n" low)                                             
                                        )
                                        (begin 
                                            (set! high (cons (car l) high ) )
                                            (format #t "\thigh = ~a\n" high)                                 
                                        )
                                    )
                                    (sort_loop (cdr l) pivot low high)             
                                )   
                            )          
                        )
                    )
                )
            )                     
        )
    )
)

; (format #t "~s\n" (list-ref data 1) )
(format #t "Quicksort: ~a\n" (quicksort data) )
