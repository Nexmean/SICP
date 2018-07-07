#lang planet neil/sicp
; вариант Алисы возвращает тот же результат.
; кроме того её вариант имеет порядки роста, если думать, что на числах разных величин операции * и reminder имеют
; одинаковую эффективность
; вариант exp-mod хорош тем, что на каждой итерации мы имеем число не больше m, от этого операции reminder и * выполняются
; быстрее
; с другой стороны exp-mod затратнее по памяти так как имеет отложенные вычисления, которые не позволяют
; провести оптимизацию хвостовой рекурсии