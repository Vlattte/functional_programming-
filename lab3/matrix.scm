(define (matrix mat)
    (let ((A (car mat) ) (B (car (cdr mat)) ))
        ; получение элемента
        (define (Access row col)             
            (cond
                ((= col 0) (car ( GetRow row)) )
                ((= col 1) (car (cdr ( GetRow row)) ) )
            )                        
        )
        
        ; получение строки по номеру
        (define (GetRow r_num) 
            (cond 
                ( (= r_num 0) A )   ; первая строка
                ( (= r_num 1) B )   ; вторая строка
            )
        )

        ; получение столбца по номеру
        (define (GetCol c_num) 
           (cond 
                ( (= c_num 0) (list (car A) (car B) ) ) ; первый столбец
                ( (= c_num 1) (list (car (cdr A)) (car (cdr B) ) ) ) ; второй столбец
            )
        )
        ; сложение матриц
        (define (Plus A2)            
            (matrix (list (map + A (A2 'GetRow 0) ) (map + B (A2 'GetRow 1)) ) )
        )

        ; произведение матриц
        (define (Prod A2)            
            (matrix (list 
                        (list
                            (apply + ( map * A (A2 'GetCol 0) ) ) 
                            (apply + ( map * A (A2 'GetCol 1) ) ) )
                        (list
                            (apply + ( map * B (A2 'GetCol 0) ) ) 
                            (apply + ( map * B (A2 'GetCol 1) ) ) )
                    ) 
            )
        )

        ; транспонирование матрицы
        (define (Transposition)
            (matrix (list (GetCol 0) (GetCol 1) ) )
        )

        ; уножение на число
        (define (ProdNum num)
            (matrix (list (map * (list num num) (GetRow 0) ) (map * (list num num) (GetRow 1)) ))
        )

        ; уножение на вектор
        (define (ProdVec vec)
                (matrix 
                    (list
                        (apply + ( map * vec (GetCol 0)) ) 
                        (apply + ( map * vec (GetCol 1)) ) 
                    )                          
                )        
        )

        ; вывод матрицы
        (define (Display)
            (format #t "Matrix:\n~a\n~a\n" A B)
        )

        ; (Display)

        (lambda args
            (apply 
                (case (car args)
                    ((Access) Access)
                    ((GetCol) GetCol)                    
                    ((GetRow) GetRow)
                    ((Plus) Plus)
                    ((Prod) Prod)
                    ((ProdNum) ProdNum)
                    ((ProdVec) ProdVec)
                    ((Transposition) Transposition)
                    ((Display) Display)
                    (else (begin (display "Invalid method\n") (exit 1)))
                )
                (cdr args)
            )
        )        
    )
    
)


(define A1 (matrix (list '(1 2) '(3 4) ) ))
(define A2 (matrix (list '(10 20) '(30 40) ) ))

; (format #t "~a\n" (A1 'GetRow 1))
; (format #t "~a\n" (A1 'GetCol 1))
; ((A1 'Plus A2) 'Display)
; ((A1 'Prod A2) 'Display)
; (A1 'Display)
; ((A1 'Transposition) 'Display)
; ((A1 'ProdNum 4) 'Display)
; ((A2 'ProdVec '(1 2)) 'Display)
; ((A2 'ProdVec '(1 2)) 'Display)
(display (A1 'Access 1 1)) (newline)

