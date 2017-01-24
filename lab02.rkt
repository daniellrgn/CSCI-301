#lang r5rs
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
    (calc-denom n d k 1.0)))

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
    (equal? 1.0 (/ x 3.0))))

(define euler-d
  (lambda (x)
    (if (divis-by-3 (+ x 1.0))
        (* 2.0 (/ (+ x 1.0) 3.0))
        1.0)))

(define stop?
  (lambda (k l) (>= l k)))

(define keep-going
  (lambda (n d k l)
     (/ (n l) (+ (d l) (calc-denom n d k (+ l 1.0))))))

(define calc-denom
  (lambda (n d k l)
    (if (stop? k l)
        (/ (n l) (d l))
        (keep-going n d k l))))
