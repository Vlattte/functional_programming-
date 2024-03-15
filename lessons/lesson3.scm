; apply применяет операцию к каждому элементу списка
;(apply sin (list 1 2 3 4 5))

(apply + (list 1 2 3 4 5 6))

(apply * (list 1 2 3 4 5 6))


; for-each применяет оператор к каждому элементу списка
(for-each display (list 1 2 3 4 5))

(display "\n")

(for-each (lambda (x) (format #t "~a\n" x)) (list 1 2 3 4 5))


(define (make-v2 x y)
    (let ((X x) (Y y))
        (define (GetX) X)
        (define (GetY) Y)
        (define (Plus v2)        
            (make-v2 (+ (v2 'GetX)  X) (+ (v2 'GetY) Y))        
        )
        (define (Scalar v2)
            (+ (* (v2 'GetX) X) (* (v2 'GetY) Y))
        )
        (define (Display) 
            (format #t "(~a ~a)" X Y)
        )

        (lambda args
            (apply 
                (case (car args)
                    ((GetY) GetY)
                    ((GetX) GetX)
                    ((Plus) Plus)
                    ((Scalar) Scalar)
                    ((Display) Display)
                    (else (begin (display "Invalid method\n") (exit 1)))
                )
                (cdr args)
            )
        )
    )
    
)

(define V1 (make-v2 1 0))
(define V2 (make-v2 0 1))

(V1 'Display) (newline)
(V2 'Display) (newline)
((V1 'Plus V2) 'Display) (newline)
(display (V1 'Scalar V2)) (newline)

