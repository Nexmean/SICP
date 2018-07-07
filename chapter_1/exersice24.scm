#lang planet neil/sicp
(#%require (only racket/base current-milliseconds))
(define (runtime) (current-milliseconds))

(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
                     m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100000)
      (report-prime n (- (runtime) start-time))
      #f))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  #t)

; так как функция random отказалась принимать большие числа пришлось сделать так, чтобы алгоритм проходил проверку Ферма
; 100000 раз чтобы замерить результат, иначе было слишком быстро

; 1009 *** 123
; 1013 *** 140
; 1019 *** 121
; 10007 *** 162
; 10009 *** 161
; 10037 *** 158
; 100003 *** 165
; 100019 *** 188
; 100043 *** 179
; 1000003 *** 191
; 1000033 *** 199
; 1000037 *** 190

; итак, время при увеличении числа на порядок увеличивается на константную величину
; значит зависимость логарифмическая