(define (f n)
  (if (< n 3) 
    n
    (+ (f (- n 1)) (f (- n 2)) (f (- n 3)))))

(define (fIter a b c count)
  (if (= count 0)
    a
    (fIter (+ a b c) a b (- count 1))))

(define (fIP n)
  (if (< n 3)
    n
    (fIter 2 1 0 (- n 2))))
; f(3) = 2 + 1 + 0
; f(4) = 3 + 2 + 1
; f(5) = 6 + 3 + 2