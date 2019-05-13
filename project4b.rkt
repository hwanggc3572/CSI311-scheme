; project 4-b
; Gyuchan Hwang
#lang racket

; sum funtion for analytic
(define sum
  (lambda (ls)
    (if (null? ls)
        0.0
        (+ (cadddr(car ls)) (sum (cdr ls)) ))))

; average function for analytic
(define average
  (lambda (ls)
    (if (null? ls)
        0.0
        (/ (sum ls) (length ls) ))))

; sum-square function for analytic (sum of the square of each element of the list)
(define sum-square
  (lambda (ls)
    (if (null? ls)
        0.0
        (+ (* (cadddr(car ls)) (cadddr(car ls))) (sum-square (cdr ls)) ))))

; average-squre function for analytic (average of sum-square)
(define average-square
  (lambda (ls)
    (if (null? ls)
        0.0
        (/ (sum-square ls) (length ls) ))))


; variance function for analytic (V(X) = E(X^2) - (E(X))^2)
(define variance
  (lambda (ls)
    (if (null? ls)
        0.0
        (- (average-square ls) (* (average ls) (average ls))) )))

; standard deviation function for analytic (sqrt(V(X)))
(define std-dev
  (lambda (ls)
    (if (null? ls)
        0.0
        (sqrt (variance ls)) )))

; minimum function for analytic
(define minimum
  (lambda (ls)
    (minimum-aux (cdr ls) (cadddr (car ls)))))

(define minimum-aux
  (lambda (ls running-min)
    (if (null? ls)
        running-min
        (if (< (cadddr (car ls)) running-min)
            (minimum-aux (cdr ls) (cadddr (car ls)))
            (minimum-aux (cdr ls) running-min)))))

; maximum function for analytic
(define maximum
  (lambda (ls)
    (maximum-aux (cdr ls) (cadddr (car ls)))))

(define maximum-aux
  (lambda (ls running-max)
    (if (null? ls)
        running-max
        (if (> (cadddr (car ls)) running-max)
            (maximum-aux (cdr ls) (cadddr (car ls)))
            (maximum-aux (cdr ls) running-max)))))

; count function for analytic
(define count
  (lambda (ls)
    (if (null? ls)
        0
        (length ls) )))

; make a time series workbench
(define make-tswb
  (lambda ()
    (let ((ls '()))
      (lambda (msg . args)
        (cond
          ((eqv? msg 'empty?) (null? ls)) ; if empty #t, else #f
          ((eqv? msg 'add!)
           (set! ls (cons (car args) ls))) ; add new data onto the stack
          ((eqv? msg 'get)
           (letrec ((sort-msg (lambda (m) (sort m (lambda (x y) (<= (car x) (car y))))))) ; for time order
             (if (null? args)
                 (sort-msg ls) ; w/o filter function
                 (sort-msg (filter (car args) ls))))) ; w/ filter function
          ((eqv? msg 'analytic) ; for analytic command, I use 9 math functions (sum, average, variance, standard deviation, minimum, maximum, count, sum-square, average-square)
           (cond
             ((null? (cdr args)) ((car args) ls)) ; analytic command w/o filter 
             ((procedure? (car args)) ((car args) (filter (car(cdr args)) ls)))
           ))
          ((eqv? msg 'clear)
           (set! ls null))
          (else "oops - incorrect msg"))))))


(define tswb (make-tswb))                                                	; make a new time series workbench 

(tswb 'empty?)                                                           		; is the db empty?
(tswb 'add!     '(2 123 "temp1"  72.1))                                  	; add some data - we will sort later
(tswb 'add!     '(1 123 "temp1"  72.0))                                  	; add some data, each is a list of 4 values
(tswb 'add!     '(2 123 "press1" 29.9213))                         	; 
(tswb 'add!     '(1 123 "press1" 29.9212))                             ;        - time 
(tswb 'add!     '(1 456 "temp1"  87.3))                                  	;        - device #
(tswb 'add!     '(1 456 "temp2"  87.4))                                  	;        - field name 
(tswb 'add!     '(1 456 "press1" 28.9234))                             ;        - field value
;(tswb 'add!     '(3 123 "temp1"  100.5))
;(tswb 'add!     '(4 123 "temp1"  50.1))
(tswb 'empty?)
(tswb 'get)                                                              		; return all records in time order
(tswb 'get      (lambda (l) (eqv? (cadr l) 456)))                      ; return records for device 456 in time order
(tswb 'get      (lambda (l) (eqv? (caddr l) "temp1")))           ; return the temp1 fields for all devices

(define (temp1-123 l)                                                    	; for convenience define a reusable filter 
  (and (eqv? (cadr l) 123) (eqv? (caddr l) "temp1")))                    

(tswb 'get      temp1-123)                                               	; get all the temp1 fields for device 123
(tswb 'analytic sum     temp1-123)                                       	; run some analytics against the filtered data
(tswb 'analytic average temp1-123)
(tswb 'analytic sum-square temp1-123)
(tswb 'analytic average-square temp1-123)
(tswb 'analytic variance temp1-123)
(tswb 'analytic std-dev temp1-123)
(tswb 'analytic minimum temp1-123)
(tswb 'analytic maximum temp1-123)
(tswb 'analytic count   temp1-123)
(tswb 'analytic sum)                                                     	; run the analytic against all data, no filter
;(tswb 'analytic average)
(tswb 'clear)
(tswb 'empty?)