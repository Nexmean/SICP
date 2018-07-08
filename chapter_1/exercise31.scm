#lang planet neil/sicp

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
      
  (iter a 1))

(define (product-recursive term a next b)
  (if (> a b)
    1
    (* (term a)
       (product-recursive term (next a) next b))))

(define (id a) a)

(define (factorial a)
  (product id 1 inc a))

(define (square x) (* x x))

(define (pi precision)
  (define (term k)
    (define 2k (* 2 k))
    (/ (square 2k)
       (* (- 2k 1) (+ 2k 1))))
  (* 2.0 (product term 1 inc precision)))