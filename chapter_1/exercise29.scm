#lang planet neil/sicp

(define (integralb f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
      dx))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (next k) (+ k 1))
  (define (y k) (f (+ a (* k h))))
  (define (term k)
    (cond ((or (= k 0) (= k n))
            (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))))

  (* (/ h 3)
     (sum term 0 next n)))

(define (cube x) (* x x x))

; > (integralb cube 0 1 (/ 1 100))
; 19999/80000
; > (integralb cube 0 1 (/ 1 1000))
; 1999999/8000000
; > (integral cube 0 1 100)
; 1/4
; > (integral cube 0 1 1000)
; 1/4