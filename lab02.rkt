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

(define cont-frac
  (lambda (n d k)
    (calc-denom n d k 1)))

(define square
  (lambda (x)
    (* x x)))

(define make-lambert-n
  (lambda (x)
    (lambda (m)
      (if (> m 1) (- (square x)) x))))

(define lambert-d
  (lambda (x)
    (- (* 2 x) 1)))

(define divis-by-3
  (lambda (x)
    (equal? 0 (modulo x 3))))

(define euler-d
  (lambda (x)
    (if (divis-by-3 (+ x 1))
        (* 2.0 (/ (+ x 1) 3.0))
        1.0)))

(define stop?
  (lambda (k l) (>= l k)))

(define keep-going
  (lambda (n d k l)
     (/ (n l) (+ (d l) (calc-denom n d k (+ l 1))))))

(define calc-denom
  (lambda (n d k l)
    (if (stop? k l)
        0.0
        (keep-going n d k l))))

;;;;;;;test cases below;;;;;;;;
;;return approximation followed by exact

;; approximate golden ratio
(display "Approximation of golden ratio: approx, exact:")
(newline)
(/ 1 (cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           100))
(display 1.6180339887498948)
(newline)

;; approximate e
(display "Approximation of e: approx, exact:")
(newline)
(+ 2 (cont-frac (lambda (x) 1.0)
             euler-d
             100))
(display (exp 1))
(newline)

;; approximate tan(3)
(display "Approximation of tan(3): approx, exact:")
(newline)
(cont-frac (make-lambert-n 3)
             lambert-d
             100)
(display (tan 3))
