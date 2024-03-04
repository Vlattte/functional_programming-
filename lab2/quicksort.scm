(define data (list 13 0 7 2 -33 4 8 91 5 6))

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
                    (begin             
                        (set! pivot (car l))
                        (let sort_loop ((l l) (pivot pivot) (low low) (high high))
                            (if (null? l)       ; элементы кончились => сортируем под массивы
                                (begin 
                                    ; если low не пустой (в high всегда хотя бы сам элемент лежит)
                                    (if (not (null? low) )
                                        (set! low (sort low))
                                    )                                    
                                    (set! high (sort high))  
                                    (concat_lists low high) ; объединяем списки
                                )
                                (begin               
                                    (if (> pivot (car l)) 
                                        (set! low (cons (car l) low ))                                           
                                        (set! high (cons (car l) high ) )
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

(format #t "Quicksort: ~a\n" (quicksort data) )
;(format #t "Quicksort: ~a\n" (concat_lists data1 data2))
