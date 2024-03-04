(define data (list 1 0 7 2 -33 4 8 91 5 6))

(define concat_lists
    (lambda (l1 l2)
        (let loop ((l1 l1) (l2 l2))
            (if (null? l1) l2
                (begin
                    ; пока в l1 больше одного элемента, обрезаем l1 и передаем дальше
                    ; на расскрутке рекурсии приписываем элементы из l1 в l2 начиная с конца
                    (if (not (null? (cdr l1)) )    ; когда остался один элемент                
                        (set! l2 (loop (cdr l1) l2))
                    )
                    (cons (car l1) l2) 
                )
            )        
        )
    )    
)

(define quicksort       
    (lambda (l)
        (let sort ((l l))                           
            (begin 
                (define pivot 0)        ; опорный элемент
                (define low '() )       ; меньше опорного элемента
                (define high '() )      ; больше опорного элемента
                
                ; тут делим по массивам low и high 
                (if (null? (cdr l)) l ; если в массиве один элемент, не сортируем          
                    (let sort_loop ((l l) (pivot (car l)) (low low) (high high))
                        (if (null? l)       ; элементы кончились => сортируем под массивы
                            ; если low не пустой (в high всегда хотя бы сам элемент лежит)
                            (if (not (null? low) )
                                (append (sort low) (sort high))
                                (append low (sort high))
                            )                                             
                            (if (> pivot (car l))
                                (sort_loop (cdr l) pivot (cons (car l) low ) high)  
                                (sort_loop (cdr l) pivot low (cons (car l) high ))                                      
                            )
                        )          
                    )
                )
            )                     
        )
    )
)

(format #t "Quicksort: ~a\n" (quicksort data) )
