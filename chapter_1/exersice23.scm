(#%require (only racket/base current-milliseconds))
(define (runtime) (current-milliseconds))

(define true #t)
(define false #f)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n) (* n n))

(define (next n)
  (if (= n 2)
    3
    (+ n 2)))

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

; > (search-for-primes 1000000000 3)
; 1000000007 *** 3
; 1000000009 *** 2
; 1000000021 *** 3
; > (search-for-primes 10000000000 3)
; 10000000019 *** 18
; 10000000033 *** 8
; 10000000061 *** 8
; > (search-for-primes 100000000000 3)
; 100000000003 *** 29
; 100000000019 *** 24
; 100000000057 *** 22

; по сравнению с результатами прошлого теста достигнуто ускорение в 1.5-2 раза, скорее всего не в 2 раза потому-что
; накладные расходы функции find-divisor увеличились по сравнению с прошлым примером