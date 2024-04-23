(define Square_Equation (lambda ( a b c )
 (let ((D (- (* b b) (* 4 a c))))
   (cond
    ((< D 0) (list ))
    ((= D 0) (list (/ (- b) (* 2 a))))
    (else (list 
            (/ (+ (- b) (sqrt D)) (* 2 a))
            (/ (- (- b) (sqrt D)) (* 2 a))
          )
    )
   )
  )
 )
)

(format #t "X^2+3X-2 : ~a\n" (Square_Equation 1 3 -2))
(format #t "X^2+2X+1 : ~a\n" (Square_Equation 1 2  1))
(format #t "X^2+2X+3 : ~a\n" (Square_Equation 1 2  3))


(define MakeAdder (lambda (N)
  (lambda (x) (+ x N))
 )
)



(define Add10  (MakeAdder 10))
(define Add100 (MakeAdder 100))
(format #t "10, 5: ~a\n" ((MakeAdder 10) 5))
(format #t "5 + 10 = ~a\n" (Add10 5))
(format #t "5 + 100 = ~a\n" (Add100 5))


; cycle 0 .. 10

(letrec ((Loop (lambda (i)
          (if (< i 10)
           (begin 
            (format #t "~a\n" i)
            (Loop (+ i 1)))
           'quit
          )
         ))
        )
 (Loop 0)
)

(let Loop( (i 0)(j 0)(k 0) )
 (cond
  ((and (< i 10)(< j 10)(< k 10)) (begin (format #t "i=~a j=~a k=~a\n" i j k) (Loop (+ i 1) j k)))
  ((and (< j 10)(< k 10))         (Loop 0 (+ j 1) k))
  ((< k 10)                       (Loop 0 0 (+ k 1)))
  (else 'quit)
 )
)




