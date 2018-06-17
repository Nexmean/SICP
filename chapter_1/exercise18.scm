;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname exercise18) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (double a) (+ a a))

(define (halve a) (floor (/ a 2)))

(define (mul a b) 
  (muli a b 0)
)

(define (muli a b i)
  (cond 
    ((= b 0) i)
    ((even? b) (muli (double a) (halve b) i))
    (else (muli a (- b 1) (+ i a)))
  )
)