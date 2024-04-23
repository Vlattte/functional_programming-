(memoize
    (define Id (lambda args body))
)

(define-syntax memoize
    (syntax-rules ()
        ((define func definition) (define func))
    )
)



; рекурсивная реализация чисел фибоначи - хороший пример (1000 элементов)
(define fib
  (lambda (n)
    (if (or (= n 1)
            (= n 2))
        1
        (+ (fib (- n 1))
           (fib (- n 2))))))
