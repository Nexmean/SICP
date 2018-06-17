;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname exercise16) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

(define (square b) (* b b))

(define (div a b) (floor (/ a b)))

; рекурсивная функция из книги
(define (expr b n)
  (cond ((= n 0)    1)
        ((even? n)  (square (expr b (/ n 2))))
        (else       (* b (expr b (- n 1))))))

; итеративное решение
(define (fast-expti num exp)
  (fast-expt-iter num exp 1))

(define (fast-expt-iter num exp acc)
  (cond
    ((= exp 1) (* acc num))
    (else
      (fast-expt-iter
        (square num)
        (div exp 2)
        (if (= (modulo exp 2) 1)
          (* acc num)
          acc
        )
      )
    )
  )
)

(define f fast-expti)

; (f 2 9) = (fi 4 4 2) = (fi 16 2 2) = (fi 256 1 2)
; (f 2 11) = (fi 4 5 2) = (fi 16 2 4) = (fi 256 1 8)