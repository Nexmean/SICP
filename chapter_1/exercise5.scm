; Бен Битобор придумал тест для проверки интерпретатора на то, с каким порядком вычислений он работает, аппликативным или нормальным. Бен определяет такие две процедуры:
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; Затем он вычисляет выражение
(test 0 (p))

; Какое поведение увидит Бен, если интерпретатор использует аппликативный порядок вычислений?
; Ответ: интерпретатор "застрянет" в рекурсии в выражении (p) и никогда не перейдёт к выполнению функции test

; Какое поведение он увидит, если интерпретатор использует нормальный порядок? Объясните Ваш ответ.
; Ответ: если интерпретатор посчитает результат выражения как ноль, а функция p вызвана не будет, так как она не нужна для того, чтобы посчитать её результат