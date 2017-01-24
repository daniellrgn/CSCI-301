#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301
;; Winter 2017
;; 
;; Lab #2
;;
;; Daniel Lorigan
;; W01163069
;;
;; The purpose of this program is to 
;; calculate continued fractions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; main function
(define cont-frac
  (lambda (n d k)
    (calc-denomenator n d k 1)))

;; returns x for first iteration, then -(x^2) thereafter
(define make-lambert-n
  (lambda (x)
    (lambda (m)
      (if (> m 1)
          (- (* x x))
          x))))

;; returns term x in the sequence of odd integers
(define lambert-d
  (lambda (x)
    (- (* 2 x) 1)))

;; returns term x of the sequence 1, 2, 1, 1, 4, 1, 1, 6,...
(define euler-d
  (lambda (x)
    (if (equal? 0 (modulo (+ x 1) 3))
        (* 2.0 (/ (+ x 1) 3.0))
        1.0)))

;; recursive iterating helper for cont-frac
(define calc-denomenator
  (lambda (n d k l)
    (if (>= l k)
        0.0
        (/ (n l) (+ (d l) (calc-denomenator n d k (+ l 1)))))))

;;;;;;;test cases below;;;;;;;;
;;return approximation followed by exact

;; approximate golden ratio
(display "Approximation of golden ratio:")
(newline)
(/ 1 (cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           100))
(display 1.6180339887498948)
(newline)

;; approximate e
(display "Approximation of e:")
(newline)
(+ 2 (cont-frac (lambda (x) 1.0)
             euler-d
             100))
(display (exp 1))
(newline)

;; approximate tan(3)
(display "Approximation of tan(3):")
(newline)
(cont-frac (make-lambert-n 3)
             lambert-d
             100)
(display (tan 3))
