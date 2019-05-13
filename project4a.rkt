; project 4-a
; Gyuchan Hwang
#lang racket

(define team  '(("Emp1" (57 57 80 47 68 56 84 65))
                ("Emp2" (57 69 57 84 87 71 77 69 61 48))
                ("Emp3" (46 47 61 65 81 64 40 77 51 78))
                ("Emp4" (70 68 89 41))
                ("Emp5" (45 48 74 83 40 44 70 85 98 86))
                ))

; return the sum of list elements
(define sum-list
  (lambda (lst)
    (if (null? lst)
        0
        (+ (car lst) (sum-list (cdr lst))))))

; return the list each element contains ("Emp", sum)
(define (getEmpTotals lst)
  (if (null? lst)
      '()
      (map (lambda (lst) (list (first lst) (sum-list (second lst)))) lst)))

; return the sum of all numbers
(define (getTeamTotal lst)
  (if (null? lst)
      0
      (sum-list (map second (getEmpTotals lst)))))
