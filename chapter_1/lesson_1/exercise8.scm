(define (cbrt x)
  (cbrtIter 0 1.0 x))

(define (cbrtIter previousGuess guess x)
  (if (goodEnough? previousGuess guess)
      guess
      (cbrtIter guess (improve guess x) x)))

(define (square x) (* x x))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (goodEnough? previousGuess guess)
  (< (abs (- previousGuess guess)) 0.0001))

