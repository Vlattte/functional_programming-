(define data (list 9 1 3 7 0 6 4 8 5 2))

(define quicksort
    (lambda (arr)
        (let loop ( (arr arr) ) ; цикл распределяющий элементы в классы меньше или больше середины массива
            (begin
                (define pivot (car arr))
                (define low '()) 
                (define high '())
                (if (not (null? (cdr arr) ) )          ; если элементов больше одного
                    (begin                       
                        (let sort_loop ( (sub_arr arr) )    ; сортируем текущий массив
                            (begin
                                (define el (car arr))
                                (if (null? sub_arr ) 'quit
                                    (begin 
                                        (if (> el pivot)            ; если элемент больше pivot  
                                            (cons high el)   
                                            (cons low el)
                                        )
                                        (sort_loop (cdr sub_arr) )   ; продолжаем сортировку
                                        (format)
                                    )                                     
                                )                                
                            )
                        )
                    )
                    (format #t "end\n")
                )
                ;(low (loop low))
                ;(high (loop high))
                ;(cons low high)
            )
        )
    )
)

;(define sort_loop (lambda (arr)
;        (begin             
;            (define pivot (car arr))
;            (let loop ( (arr arr) )
;
;            )
;        )
;    )
;)

(quicksort data)