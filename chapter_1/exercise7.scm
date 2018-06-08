(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

; (define (good-enough? guess x)
;   (< (abs (- (square guess) x)) 0.001))

; (define (sqrt-iter guess x)
;   (if (good-enough? guess x)
;       guess
;       (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 0 1.0 x))

(define (sqrt-iter previous-guess guess x)
  (if (good-enough? previous-guess guess)
      guess
      (sqrt-iter guess (improve guess x) x)))

(define (good-enough? previous-guess guess)
  (< (abs (- previous-guess guess)) 0.000000001))

; Данный вариант работает быстрее так как в функции good-enough? не вычисляется квадрата числа guess, в отличии от старого варианта