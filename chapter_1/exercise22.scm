(#%require (only racket/base current-milliseconds))
(define (runtime) (current-milliseconds))

(define true #t)
(define false #f)

; PRIME?
(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; Timed prime test
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      false))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  true)

(define (search-for-primes from count)
  (if (> count 0)
    (if (timed-prime-test from)
      (search-for-primes (+ from 1) (- count 1))
      (search-for-primes (+ from 1) count))))

; > (search-for-primes 1000 3)
; 1009 *** 0
; 1013 *** 0
; 1019 *** 1
; > (search-for-primes 10000 3)
; 10007 *** 0
; 10009 *** 0
; 10037 *** 0
; > (search-for-primes 100000 3)
; 100003 *** 0
; 100019 *** 0
; 100043 *** 0
;
; как видно, компьютер слишком быстрый, чтобы была какая-то видимая разница при выполнении алгоритма
; над числами таких порядков, возьмём числа побольше
;
; > (search-for-primes 1000000000 3)
; 1000000007 *** 5
; 1000000009 *** 4
; 1000000021 *** 4
; > (search-for-primes 10000000000 3)
; 10000000019 *** 13
; 10000000033 *** 13
; 10000000061 *** 12
; > (search-for-primes 100000000000 3)
; 100000000003 *** 42
; 100000000019 *** 34
; 100000000057 *** 31
;
; как видно при применении алгоритма к числам таких порядков разница в скорости примерно в 3 раза,
; тоесть примерно корень из 10 раз
; соответственно алгоритм на моём компьютере затрачивает времени пропорционально числу шагов