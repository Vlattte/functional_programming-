(let Loop( (grad 0) )
    (define rad (/ (* grad 3.1415926) 180 ) )
    (if (not (> grad 360) ) 
        (begin 
            (format #t "| cos(~a)=~5f \t | sin(~a)=~5f \t | tg(~a)=~5f \t |\n" grad (cos rad) grad (sin rad) grad (tan rad))
            (format #t "|========================================================================|\n") 
            (Loop (+ grad 5) ) 
        )
    )
)
