#lang racket
;; Programming Languages Homework 5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and, if necessary, change the filename
(require "hw5.rkt")

(require rackunit)

(define tests
  (test-suite
   "Sample tests for Assignment 5"
   
   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test")
   
   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "racketlist->mupllist test")

   ;; fun test
   (check-equal? (eval-exp (fun #f "x" (var "x"))) (closure '() (fun #f "x" (var "x"))))

   ;; tests if ifgreater returns (int 2)
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")
   (check-equal? (eval-exp (ifgreater (int 3) (int 2) (int 3) (int 2))) (int 3) "ifgreater test 2")
   
   ;; mlet test
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")
   
   ;; call test
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")

   ;; apair test
   (check-equal? (eval-exp (apair (int 1) (add (int 1) (int 1)))) (apair (int 1) (int 2)) "apair test")

   ;;fst test
   (check-equal? (eval-exp (fst (apair (int 1) (int 2)))) (int 1) "fst test")
   
   ;; snd test
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")
   
   ;; isaunit test
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")
   (check-equal? (eval-exp (isaunit (call (closure '() (fun #f "x" (aunit))) (int 10)))) (int 1) "isaunit test")
   
   ;; ifaunit test
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
   (check-equal? (eval-exp (ifaunit (aunit) (int 2) (int 3))) (int 2) "ifaunit test")
   (check-equal? (eval-exp (ifaunit (snd (apair (int 10) (aunit))) (int 10) (int 20))) (int 10) "ifaunit test")
   (check-equal? (eval-exp (ifaunit (fst (apair (int 10) (aunit))) (int 10) (int 20))) (int 20) "ifaunit test")
   
   ;; mlet* test
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (int 20))) (add (var "x") (var "y")))) (int 30) "mlet* test 2")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (add (var "x") (int 5))))
                                  (add (var "x") (var "y"))))
                           (int 25) "mlet* test 3")
   
   ;; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")
   (check-equal? (eval-exp (ifeq (int 1) (int 1) (int 3) (int 4))) (int 3) "ifeq test")
   (check-equal? (eval-exp (ifeq (int 2) (int 1) (int 3) (int 4))) (int 4) "ifeq test")
   
   ;; mupl-map test
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit))))
                 (apair (int 8) (aunit)) "mupl-map test")
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (var "x"))) (aunit)))
                 (aunit)
                 "mupl-map test")
   
   ;; problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist
                    (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")
   ))

(define challenge-tests
  (test-suite
   "Tests for the challenge problem"

   ;; compute-free-vars tests
   (check-equal? (compute-free-vars (int 10))
                 (int 10) "compute-free-vars in an int expression")

   (check-equal? (compute-free-vars (var "x"))
                 (var "x") "compute-free-vars in a var expression")
   (check-equal? (compute-free-vars (apair (var "x") (var "y")))
                 (apair (var "x") (var "y")) "compute-free-vars in apair expression")

   (check-equal? (compute-free-vars (add (var "x") (var "y")))
                 (add (var "x") (var "y")) "compute-free-vars in an add expression")
   (check-equal? (compute-free-vars (add (int 1) (int 2)))
                 (add (int 1) (int 2)) "compute-free-vars in an add expression with no vars")

   (check-equal? (compute-free-vars (ifgreater (var "x") (var "y") (var "z") (aunit)))
                 (ifgreater (var "x") (var "y") (var "z") (aunit)) "compute-free-vars in an ifgreater expression")

   (check-equal? (compute-free-vars (fun "foo" "x" (add (var "x") (var "y"))))
                 (fun-challenge "foo" "x" (add (var "x") (var "y")) (set "y")) "compute-free-vars in a fun expression")
   (check-equal? (compute-free-vars (fun "foo" "x" (fun "bar" "y" (var "z"))))
                 (fun-challenge "foo" "x" (fun "bar" "y" (var "z")) (set "z")) "compute-free-vars in a nested fun expression")
   (check-equal? (compute-free-vars (fun #f "x" (var "y")))
                 (fun-challenge #f "x" (var "y") (set "y")) "compute-free-vars in a fun expression")

   (check-equal? (compute-free-vars (call (fun "foo" "x" (var "y")) (int 10)))
                 (call (fun "foo" "x" (var "y")) (int 10)) "compute-free-vars in a call expression")

   (check-equal? (compute-free-vars (mlet "x" (int 10) (var "y")))
                 (mlet "x" (int 10) (var "y")) "compute-free-vars in a mlet expression")

   (check-equal? (compute-free-vars (fst (apair (int 1) (int 2))))
                 (fst (apair (int 1) (int 2))) "compute-free-vars in a fst expression")

   (check-equal? (compute-free-vars (snd (apair (int 1) (int 2))))
                 (snd (apair (int 1) (int 2))) "compute-free-vars in a snd expression")

   (check-equal? (compute-free-vars (aunit))
                 (aunit) "compute-free-vars in aunit expression")

   (check-equal? (compute-free-vars (isaunit (var "x")))
                 (isaunit (var "x")) "compute-free-vars in an isaunit expression")
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
(run-tests challenge-tests)
