#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301
;; Spring 2015
;; 
;; Lab #1
;;
;; My Name Here
;; W012345678
;;
;; The purpose of this program is to 
;; bla bla bla
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define first-empty?
  (lambda (ls1 ls2)
    (cond ((null? (cdr ls1)) '())
          ((null? (cdr ls2)) 0)
          (else (first-empty? (cdr ls1) (cdr ls2))))))

(define shorter
  (lambda (ls1 ls2)
    (cond ((null? ls1) ls1)
          ((null? ls2) ls2)
          ((null? (first-empty? ls1 ls2)) ls1)
          (else ls2))))

;;;;;;;test cases below;;;;;;;;
(define longList
  (list 1 2 3 4 5 6 7 8 9))
(define shortList
  (list 1 2 3))
(define nullList
  (list '()))
(define emptyList
  (list))

(shorter longList shortList)

(shorter shortList longList)

(shorter longList emptyList)

(shorter longList longList)

(shorter emptyList nullList)



