#lang r5rs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301
;; Spring 2015
;; 
;; Lab #1
;;
;; Daniel Lorigan
;; W01163069
;;
;; The purpose of this program is to 
;; bla bla bla
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(display "Hello Daniel, you genius!")
;;(newline)
;;(display "You look fabulous today!")
;;(newline)
(define make-pi
  (lambda (x)
    (calc-pi 4.0 1.0 0.0 x)))

(define check-result
  (lambda (numer denom x)
    (< (abs (/ numer denom)) x)
    ))

(define calc-pi
  (lambda (numer denom result x)
    (if (check-result numer denom result x)
        ))