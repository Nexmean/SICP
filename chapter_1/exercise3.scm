(define (square x)
  (* x x))

(define (<= a b) (not (> a b)))
(define (>= a b) (not (< a b)))

(define (sumOfBiggestSquares a b c)
  (cond ((and (<= a b) (<= a c)) (+ (square b) (square c)))
        ((and (<= b c) (<= b a)) (+ (square a) (square c)))
        (else (+ (square a) (square b)))))