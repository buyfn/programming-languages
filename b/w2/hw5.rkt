;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; a
(define (racketlist->mupllist racketlist)
  (if (null? racketlist)
      (aunit)
      (apair (car racketlist)
             (racketlist->mupllist (cdr racketlist)))))

;; b
(define (mupllist->racketlist mupllist)
  (if (aunit? mupllist)
      null
      (cons (apair-e1 mupllist)
            (mupllist->racketlist (apair-e2 mupllist)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "Tried to compare two values that are not both integers")))]
        [(fun? e)
         (closure env e)]
        [(mlet? e)
         (let ([var (mlet-var e)]
               [val (eval-under-env (mlet-e e) env)]
               [body (mlet-body e)])
           (eval-under-env body (cons (cons var val) env)))]
        [(call? e)
         (let ([c (eval-under-env (call-funexp e) env)]
               [arg-value (eval-under-env (call-actual e) env)])
           (if (closure? c)
               (let* ([f (closure-fun c)]
                      [arg-name (fun-formal f)]
                      [extended-env (cons (cons arg-name arg-value)
                                          (closure-env c))])
                 (eval-under-env (fun-body f)
                                 (if (fun-nameopt f)
                                     (cons (cons (fun-nameopt f) c) extended-env)
                                     extended-env)))
               (error "MUPL call applied to something that is not a closure")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "Applied fst to something that is not apair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "Applied snd to something that is not apair")))]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([v (car (car lstlst))]
            [e (cdr (car lstlst))])
        (mlet v e (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun #f "f"
       (fun "iter" "xs" (ifaunit (var "xs")
                                 (aunit)
                                 (apair (call (var "f") (fst (var "xs")))
                                        (call (var "iter") (snd (var "xs"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i" (fun #f "xs" (call (call (var "map") (fun #f "x" (add (var "x") (var "i")))) (var "xs"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

(define (compute-free-vars e)
  (struct res (e fvs))
  (define (f e)
    (cond [(var? e) (res e (set (var-string e)))]
          [(int? e) (res e (set))]
          [(aunit? e) (res e (set))]
          [(isaunit? e)
           (res e (res-fvs (f (isaunit-e e))))]
          [(apair? e)
           (res e (set-union (res-fvs (f (apair-e1 e)))
                             (res-fvs (f (apair-e2 e)))))]
          [(add? e)
           (res e (set-union (res-fvs (f (add-e1 e)))
                             (res-fvs (f (add-e2 e)))))]
          [(fst? e)
           (res e (res-fvs (f (fst-e e))))]
          [(snd? e)
           (res e (res-fvs (f (snd-e e))))]
          [(ifgreater? e)
           (res e (set-union (res-fvs (f (ifgreater-e1 e)))
                             (res-fvs (f (ifgreater-e2 e)))
                             (res-fvs (f (ifgreater-e3 e)))
                             (res-fvs (f (ifgreater-e4 e)))))]
          [(fun? e)
           (let* ([body-fvs (res-fvs (f (fun-body e)))]
                  [fvs (set-remove (set-remove body-fvs (fun-nameopt e)) (fun-formal e))])
             (res (fun-challenge (fun-nameopt e) (fun-formal e) (fun-body e) fvs) fvs))]
          [(call? e)
           (res e (set-union (res-fvs (f (call-funexp e)))
                             (res-fvs (f (call-actual e)))))]
          [(mlet? e)
           (res e (set-union (res-fvs (f (mlet-e e)))
                             (set-remove (res-fvs (f (mlet-body e))) (mlet-var e))))]
          [else (error (format "bad MUPL expression: ~v" e))]))
  (res-e (f e)))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "Tried to compare two values that are not both integers")))]
        [(fun-challenge? e)
         (closure (set-map (fun-challenge-freevars e)
                           (lambda (v) (cons v (envlookup env v))))
                  e)]
        [(mlet? e)
         (let ([var (mlet-var e)]
               [val (eval-under-env (mlet-e e) env)]
               [body (mlet-body e)])
           (eval-under-env body (cons (cons var val) env)))]
        [(call? e)
         (let ([c (eval-under-env (call-funexp e) env)]
               [arg-value (eval-under-env (call-actual e) env)])
           (if (closure? c)
               (let* ([f (closure-fun c)]
                      [arg-name (fun-formal f)]
                      [extended-env (cons (cons arg-name arg-value)
                                          (closure-env c))])
                 (eval-under-env (fun-body f)
                                 (if (fun-nameopt f)
                                     (cons (cons (fun-nameopt f) c) extended-env)
                                     extended-env)))
               (error "MUPL call applied to something that is not a closure")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "Applied fst to something that is not apair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "Applied snd to something that is not apair")))]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
