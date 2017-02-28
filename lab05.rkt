#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301
;; Winter 2017
;; 
;; Lab #5
;;
;; Daniel Lorigan
;; W01163069
;;
;; The purpose of this program is to 
;; convert words to pig latin.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define vowel?
  (lambda (letter)
    (member letter (list #\a #\e #\i #\o #\u))))

(define piglatin
    (lambda (sentence)
    (string-join (topl (string-split sentence) '())))) ;; split sentence up

(define topl
  (lambda (wordls plwls)
    (cond ((null? wordls) plwls)
          (else (topl (cdr wordls) (append plwls (list (jumble (string->list (car wordls)) '()))))))))

(define jumble
  (lambda (charls appls)
    (cond ((vowel? (car charls)) (list->string (append charls (append appls '(#\w #\a #\y)))))
          (else (list->string (append (rearr charls appls) '(#\a #\y)))))))

(define rearr
  (lambda (charls appls)
    (cond ((vowel? (car charls)) (append charls appls))
          (else (rearr (cdr charls) (append appls (list (car charls))))))))
        
;;;;;;;;; test cases ;;;;;;;;;;;

(piglatin "hello there you gorgeous thing are you busy tonight")
(piglatin "insane in the membrane insane in the brain")
(piglatin "four score and seven years ago our fathers brought forth on this continent a new nation conceived in liberty and dedicated to the proposition that all men are created equal now we are engaged in a great civil war testing whether that nation or any nation so conceived and so dedicated can long endure we are met on a great battlefield of that war we have come to dedicate a portion of that field as a final resting place for those who here gave their lives that that nation might live it is altogether fitting and proper that we should do this")



    


