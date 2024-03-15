(define (matrix mat)
    (let ((A (car mat) ) (B (cdr mat) ))
        (define (GetRow r_num) 
            (cond 
                ((= r_num 0) A)
                ((= r_num 1) B)
            )
        )
        (define (GetCol c_num) 
           (cond 
                ( (= c_num 0) (append (append (car A)) (car B) ) )
                ( (= c_num 1) (append (append (cdr A)) (cdr B) ) )
            )
        )

        (lambda args
            (apply 
                (case (car args)
                    ((GetCol) GetCol)                    
                    ((GetRow) GetRow)
                    (else (begin (display "Invalid method\n") (exit 1)))
                )
                (cdr args)
            )
        )
    )
    
)


(define A1 (matrix '( '(1 2) '(3 4) ) ))
(format #t "~a\n" (A1 'GetRow 1))
(format #t "~a\n" (A1 'GetCol 0))
; (define A2 (matrix 0 1))

