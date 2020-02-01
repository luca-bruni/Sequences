;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sequences) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; **********************************************
;; Luca Bruni (20794576)
;; CS 135 Fall 2018
;; Assignment 09, Problem 3
;; **********************************************
;;

;; ============== Part A ============== ;;


;; (solution? f lon) checks that f generates the values in
;;    the list of integers lon, where f takes a position in
;;    the list as an argument, starting at 0
;; solution?: (Nat -> Int) (listof Int) -> Bool
;; Examples:
(check-expect (solution? (lambda (i) (* 3 (+ i 1)))
                         (list 3 6 9 12 15 18)) true)
(check-expect (solution? (lambda (i) (add1 i))
                         (list 1 2 3 4 5 6 7)) true)

(define
  (solution? f lon)
  (equal? lon (build-list (length lon) f)))

;; Tests:

(check-expect (solution? (lambda (i) (* 3 (+ i 1)))
                         (list 1 2 3 4 5 6 7)) false)
(check-expect (solution? (lambda (i) (* 3 (+ i 1))) empty) true)

;; ============== Part B ============== ;;


;; (guess-quadratic lon) guesses that the number sequence lon has a
;;    quadratic solution and produces the guess
;; guess-quadratic: (listof Int) -> (Nat -> Int)
;; Examples:

(check-expect (solution? (guess-quadratic empty) '(1)) false)
(check-expect (solution? (guess-quadratic '(1)) '(1)) true)

(define
  (guess-quadratic lon)
  (cond [(empty? lon) (lambda (i) 0)]
        [(= (length lon) 1)
         (lambda (i) (first lon))]
        [(= (length lon) 2)
         (lambda (i) (+ (* (- (second lon) (first lon)) i)
                        (first lon)))]
        [else
         (lambda (i) (+ (* (sqr i)
                           (/ (- (+ (third lon) (first lon))
                                 (* 2 (second lon))) 2))
                        (* i (- (* 2 (- (second lon) (first lon)))
                                (/ (- (third lon) (first lon)) 2)))
                        (first lon)))]))

;; Tests:

(check-expect (solution? (guess-quadratic empty) '(0 0)) true)
(check-expect (solution? (guess-quadratic empty) empty) true)
(check-expect (solution? (guess-quadratic '(1 2)) '(1 2)) true)
(define test-a '(3 5 8 11 14 17))
(check-expect (solution? (guess-quadratic test-a) test-a) false)
(define test-b '(0 2 4 6 8 10))
(check-expect (solution? (guess-quadratic test-b) test-b) true)

;; ============== Part C ============== ;;


;; (try-quadratic lon) tries solving the number sequence lon with
;;     a quadratic. It produces the solution if it works and empty
;;     if it doesn't
;; try-quadratic: (listof Int) -> (anyof (Nat -> Int) empty)
;; Examples:
(check-expect ((try-quadratic test-b) 0) 0)
(check-expect ((try-quadratic test-b) 1) 2)

(define
  (try-quadratic lon)
  (cond [(solution? (guess-quadratic lon) lon)
         (guess-quadratic lon)]
        [else empty]))

;; Tests:

(check-expect ((try-quadratic test-b) 6) 12)
(check-expect ((try-quadratic empty) 6) 0)
(check-expect (try-quadratic test-a) empty)

;; ============== Part D ============== ;;


;; (guess-recursive lon) produces a function based on the inputted lon
;; guess-recursive: (listof Int) -> (Nat -> Int)
;; Examples:
(check-expect ((try-recursive '(0 0 0 0 0 0)) 1000) 0)
(check-expect ((try-recursive '(10 20 30 40 50 60)) 9) 100)

(define
  (guess-recursive lon)
  (local [(define (rec n a b)
          (cond [(= n 0) (first lon)]
                [(= n 1) (second lon)]
                [else
                 (local [(define (rec/acc n c d)
                         (cond [(= n 2) (+ (* a d)
                                           (* b c))]
                               [else (rec/acc (sub1 n) d
                                              (+ (* a d)
                                                 (* b c)))]))]
                   (rec/acc n (first lon) (second lon)))]))]
    (cond [(empty? lon) (lambda (i) 0)]
          [(= (length lon) 1) (lambda (i) (first lon))]
          [(= (length lon) 2) (lambda (i) (cond [(odd? i) (second lon)]
                                                [else (first lon)]))]
          [(= (length lon) 3) (guess-recursive (append lon (list 3)))]
          [(and (zero? (first lon)) (zero? (second lon))) (lambda (i) 0)]
          [(and (zero? (first lon)) (zero? (third lon)))
           (lambda (i) (rec i 0
                         (/ (fourth lon) (second lon))))]
          [(and (zero? (second lon)) (zero? (third lon)))
           (lambda (i) (rec i 0 0))]
          [(and (zero? (first lon)) (not (zero? (third lon))))
           (lambda (i) (rec i
                         (/ (third lon) (second lon))
                         (- (/ (fourth lon) (second lon))
                            (sqr (/ (third lon) (second lon))))))]
          [(and (zero? (second lon)) (not (zero? (third lon))))
           (lambda (i) (rec i
                         (/ (fourth lon) (third lon))
                         (/ (third lon) (first lon))))]
          [(zero? (- (sqr (second lon))
                     (* (third lon)
                        (first lon)))) (lambda (i)
                                         (* (first lon)
                                            (expt (/ (second lon)
                                                     (first lon)) i)))]
          [else
           (lambda (i) (rec i
                         (/ (- (* (third lon) (second lon))
                               (* (fourth lon) (first lon)))
                            (- (sqr (second lon))
                               (* (third lon) (first lon))))
                         (/ (- (third lon)
                               (* (second lon)
                                  (/ (- (* (third lon) (second lon))
                                        (* (fourth lon) (first lon)))
                                     (- (sqr (second lon))
                                        (* (third lon) (first lon))))))
                            (first lon))))])))               

;; Tests:

(define test-d (list 1 1 2 3 5 8 13))
(check-expect ((try-recursive test-d) 8) 34)
(check-expect ((try-recursive test-d) 101) 927372692193078999176)
(define test-e (list 0 1 1 2 3 5 8))
(check-expect ((try-recursive test-e) 8) 21)
(define test-f (list 1 1 3 5 11 21 43))
(check-expect ((try-recursive test-f) 8) 171)
(define test-g (list 1 0 0 0 0 0))
(check-expect ((try-recursive test-g) 0) 1)
(check-expect ((try-recursive test-g) 1000) 0)
(check-expect ((try-recursive empty) empty) 0)
(check-expect ((try-recursive '(1)) '(1)) 1)
(check-expect (solution? (guess-recursive '(10 20)) '(10 20)) true)
(check-expect (solution? (guess-recursive '(1 2 3)) '(1 2 3)) true)
(check-expect ((try-recursive '(0 0 0 0)) 0) 0)
(check-expect ((try-recursive '(0 2 0 0)) 0) 0)
(check-expect ((try-recursive '(1 0 1 0)) 0) 1)
(check-expect ((try-recursive '(1 2 4 8 16 32)) 10) 1024)
(check-expect (try-recursive test-a) empty)


;; (try-recursive lon) checks the function returned by guess-recursive based
;;    on the inputted lon
;; try-recursive: (listof Int) -> (anyof (Nat -> Int) empty)
;; Examples:
(check-expect ((try-recursive '(1 0 1 0)) 4) 1)
(check-expect ((try-recursive '(1 0 1 0)) 5) 0)

(define
  (try-recursive lon)
  (cond [(solution? (guess-recursive lon) lon)
         (guess-recursive lon)]
        [else empty]))

;; Tests:

(check-expect ((try-recursive test-d) 7) 21)
(check-expect ((try-recursive test-g) 0) 1)
(check-expect ((try-recursive test-g) 1000) 0)
(check-expect (try-recursive test-a) empty)
(check-expect ((try-recursive empty) 100000) 0)

;; ============== Part E ============== ;;


;; (solve lon) tries quadratic and recursive patterns, and gives a solution
;; solve: (listof Int) -> (Nat -> Int)
;; Examples:
(check-expect (solve test-a) empty)
(check-expect ((solve test-b) 10) 20)

(define (solve lon)
  (local [(define (try lst)
            (cond [(empty? lst) empty]
                  [(empty? ((first lst) lon)) (try (rest lst))]
                  [else ((first lst) lon)]))]
    (try (list try-quadratic try-recursive))))

;; tests:

(check-expect ((solve test-g) 0) 1)
(check-expect ((solve test-g) 1000000) 0)
(check-expect ((solve test-d) 101) 927372692193078999176)
             