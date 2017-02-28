#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301
;; Winter 2017
;; 
;; Lab #6
;;
;; Daniel Lorigan
;; W01163069
;;
;; The purpose of this program is to
;; convert numbers to english words.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define num-split
  (lambda (ls x)
    (cond ((> x 0)
           (num-split (append ls (list (remainder x 1000))) (quotient x 1000)))
          (else ls))))

(define name-combine
  (lambda (wls nls rls)
    (cond ((empty? nls)
           rls)
          ((equal? (first nls) 0)
           (name-combine (remove (first wls) wls) (remove (first nls) nls) rls))
          (else (name-combine (remove (first wls) wls) (remove (first nls) nls) (append (list (first nls)) (list (first wls)) rls))))))
  
(define number-name
  (lambda (x)
    (define namels (list 'thousand 'million 'billion 'trillion 'quadrillion 'quintillion 'sextillion 'septillion 'octillion 'nonillion 'decillion
                         'undecillion 'duodecillion 'tredecillion 'quattuordecillion 'quindecillion 'sexdecillion 'septendecillion 'octodecillion 'novemdecillion
                         'vigintillion 'unvigintillion 'duovigintillion 'tresvigintillion 'quattuorvigintillion 'quinquavigintillion 'sesvigintillion 'septembigintillion 'octovigintillion 'novembigintillion
                         'trigintillion 'untrigintillion 'duotrigintillion 'trestrigintillion)) ;;why not?
    (define numls (num-split '() x))
    (if (>= x (expt 10 105)) (display (string-append "** Error: number is too large to process."))
    (cond ((equal? numls '())
           '())
          ((equal? (first numls) 0)
           (name-combine namels (remove (first numls) numls) (list)))
          (else (name-combine namels (remove (first numls) numls) (list (first numls))))))))


;;;;;;;; test cases ;;;;;;;;;

(display "Tests:")(newline)
(number-name 1234567890)
(number-name 1000000)
(number-name 1000000001)
(number-name 2423000000000000002344)
(display "Zero: ") (number-name 0)
(display "A big number: ") (number-name 1000000000000000000000000)
(display "Our biggest number: ") (number-name 999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999)
(display "Too big?: ") (number-name (- (expt 10 903) 1))