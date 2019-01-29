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

; 7
(define (stream-add-zero s)
  (letrec ([f (lambda (stream)
                (let* ([p (stream)]
                       [v (car p)]
                       [next (cdr p)])
                  (cons (cons 0 v)
                        (lambda () (f next)))))])
    (lambda () (f s))))

; 8
(define (cycle-lists xs ys)
  (define (aux n)
    (cons (list-nth-mod xs n)
          (list-nth-mod ys n)))
  (letrec ([f (lambda (x) (cons (aux x)
                                (lambda () (f (add1 x)))))])
    (lambda () (f 0))))

; 9
(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [aux (lambda (i) (if (>= i len)
                                #f
                                (let ([current (vector-ref vec i)])
                                  (if (pair? current)
                                      (if (equal? (car current) v)
                                          current
                                          (aux (add1 i)))
                                      (aux (add1 i))))))])
    (aux 0)))

; 10
(define (cached-assoc xs n)
  (define cache (make-vector n #f))
  (define pos 0)
  (lambda (v) (let ([cached-result (vector-assoc v cache)])
                (if cached-result
                    cached-result
                    (let ([result (assoc v xs)])
                      (if result
                          (begin
                            (vector-set! cache pos result)
                            (set! pos (if (>= pos (sub1 n)) 0 (add1 pos)))
                            result)
                          result))))))

; 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less limit do body)
     (let ([l limit]
           [b body])
       (define (loop i)
         (if (>= i l)
             #t
             (begin
               body
               (loop (add1 i)))))
       (loop b))]))
