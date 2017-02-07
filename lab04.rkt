#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301
;; Winter 2017
;; 
;; Lab #4
;;
;; Daniel Lorigan
;; W01163069
;;
;; The purpose of this program is to 
;; calculate function compositions and
;; repeated function compositions using
;; both recursion and tail recursion.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define repeated
  (lambda (f n)
    (if (> n 1)
        (compose f (repeated f (- n 1)))
        f)))

(define tail_repeated
  (lambda (f n)
    (function1 f n f)))

(define function1
  (lambda (f n runval)
    (if (= n 1)
        runval
        (function1 f (- n 1) (compose f runval)))))
      
;;;;;;;;;;tests;;;;;;;;;;
(define square
  (lambda (x) (* x x)))

(define add1
  (lambda (x) (+ x 1)))
(display "compose tests:")
(newline)
(display "(5+1)^2 = ")((compose square add1) 5) ;;36
(display "(5^2)+1 = ")((compose add1 square) 5) ;;26
(display "recursion tests:")
(newline)
(display "5^3 = ")((repeated square 2) 5) ;;625
(display "5^4 = ")((repeated square 3) 5) ;;390625
(display "50 + (50)1 = ")((repeated add1 50) 50) ;;100
(display "tail recursion tests:")
(newline)
(display "5^3 = ")((tail_repeated square 2) 5) ;;625
(display "5^4 = ")((tail_repeated square 3) 5) ;;390625
(display "50 + (50)1 = ")((tail_repeated add1 50) 50) ;;100

