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

(define onespl
  (lambda (x)
    (remainder x 10)))

(define tenspl
  (lambda (x)
    (quotient (remainder x 100) 10)))

(define hundpl
  (lambda (x)
    (quotient x 100)))

(define ones
  (lambda (x)
    (cond ((equal? (onespl x) 0) '())
          ((equal? (onespl x) 1) (list 'one))
          ((equal? (onespl x) 2) (list 'two))
          ((equal? (onespl x) 3) (list 'three))
          ((equal? (onespl x) 4) (list 'four))
          ((equal? (onespl x) 5) (list 'five))
          ((equal? (onespl x) 6) (list 'six))
          ((equal? (onespl x) 7) (list 'seven))
          ((equal? (onespl x) 8) (list 'eight))
          ((equal? (onespl x) 9) (list 'nine)))))

(define teens?
  (lambda (x)
    (equal? (tenspl x) 1)))

(define teens
  (lambda (x)
    (cond ((equal? (onespl x) 0) (list 'ten))
          ((equal? (onespl x) 1) (list 'eleven))
          ((equal? (onespl x) 2) (list 'twelve))
          ((equal? (onespl x) 3) (list 'thirteen))
          ((equal? (onespl x) 4) (list 'fourteen))
          ((equal? (onespl x) 5) (list 'fifteen))
          ((equal? (onespl x) 6) (list 'sixteen))
          ((equal? (onespl x) 7) (list 'seventeen))
          ((equal? (onespl x) 8) (list 'eighteen))
          ((equal? (onespl x) 9) (list 'nineteen)))))

(define tens
  (lambda (x)
    (cond ((equal? (tenspl x) 0) '())
          ((equal? (tenspl x) 2) (list 'twenty))
          ((equal? (tenspl x) 3) (list 'thirty))
          ((equal? (tenspl x) 4) (list 'fourty))
          ((equal? (tenspl x) 5) (list 'fifty))
          ((equal? (tenspl x) 6) (list 'sixty))
          ((equal? (tenspl x) 7) (list 'seventy))
          ((equal? (tenspl x) 8) (list 'eighty))
          ((equal? (tenspl x) 9) (list 'ninety)))))

(define hundreds
  (lambda (x)
    (if (> x 99) (append (ones (hundpl x)) (list 'hundred))
        '())))

(define name<1000
  (lambda (x)
    (if (equal? x 0) (list 'zero)
        (if (teens? x) (append (hundreds x) (teens x))
            (append (hundreds x) (tens x) (ones x))))))

(define num-split
  (lambda (ls x)
    (cond ((> x 0)
           (num-split (append ls (list (remainder x 1000))) (quotient x 1000)))
          (else ls))))

(define name-combine
  (lambda (wls nls rls)
    (cond ((empty? nls) ;;base
           rls)
          ((equal? (first nls) 0)
           (name-combine (remove (first wls) wls) (remove (first nls) nls) rls)) ;;skip powers of 1000 with no sig figs
          (else (name-combine (remove (first wls) wls) (remove (first nls) nls) (append (name<1000 (first nls)) (list (first wls)) rls))))))
  
(define number-name
  (lambda (x)
    (define namels (list 'thousand 'million 'billion 'trillion 'quadrillion 'quintillion 'sextillion 'septillion 'octillion 'nonillion
                         'decillion 'undecillion 'duodecillion 'tredecillion 'quattuordecillion 'quindecillion 'sexdecillion 'septendecillion 'octodecillion 'novemdecillion
                         'vigintillion 'unvigintillion 'duovigintillion 'tresvigintillion 'quattuorvigintillion 'quinquavigintillion 'sesvigintillion 'septembigintillion 'octovigintillion 'novembigintillion
                         'trigintillion 'untrigintillion 'duotrigintillion 'trestrigintillion)) ;;why not?
    (define numls (num-split '() x))
    (if (>= x (expt 10 105)) (display "** Error: number is too large to process.")
    (cond ((equal? numls '())
           '())
          ((equal? (first numls) 0)
           (name-combine namels (remove (first numls) numls) (list)))
          (else (name-combine namels (remove (first numls) numls) (name<1000 (first numls))))))))


;;;;;;;; test cases ;;;;;;;;;
(display "Tests:")(newline)
(number-name 5513345)
(number-name 2432902008176640000)
(number-name 1000000001)
(number-name 1234567890)
(display "Zero: ") (number-name 0)
(display "A big number: ") (number-name 1000000000000000000000000)
;;(display "A very big number: ") (number-name 999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999)
(display "Too big?: ") (number-name (expt 10 1000))

