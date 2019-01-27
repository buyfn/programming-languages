#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; 1
(define (sequence low high stride)
  (if (< high low)
      null
      (cons low
            (sequence (+ low stride) high stride))))

;2
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

;3
(define (list-nth-mod xs n)
  (let* ([len (length xs)]
         [i (remainder n len)])
    (cond
      [(< n 0) (error "list-nth-mod: negative number")]
      [(null? xs) (error "list-nth-mod: empty list")]
      [else (car (list-tail xs i))])))