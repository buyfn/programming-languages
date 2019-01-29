#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; 1
(define (sequence low high stride)
  (if (< high low)
      null
      (cons low
            (sequence (+ low stride) high stride))))

; 2
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

; 3
(define (list-nth-mod xs n)
  (let* ([len (length xs)]
         [i (remainder n len)])
    (cond
      [(< n 0) (error "list-nth-mod: negative number")]
      [(null? xs) (error "list-nth-mod: empty list")]
      [else (car (list-tail xs i))])))

; 4
(define (stream-for-n-steps s n)
  (if (<= n 0)
      null
      (let* ([p (s)]
             [v (car p)]
             [next (cdr p)])
        (cons v (stream-for-n-steps next (sub1 n))))))

; 5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (- 0 x) x)
                                (lambda () (f (add1 x)))))])
    (lambda () (f 1))))

; 6
(define dan-then-dog
  (letrec ([f (lambda (last) (let ([next (if (equal? last "dan.jpg")
                                             "dog.jpg"
                                             "dan.jpg")])
                               (cons next (lambda () (f next)))))])
    (lambda () (f "dog.jpg"))))