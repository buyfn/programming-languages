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
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (int 20))) (add (var "x") (var "y")))) (int 30) "mlet* text 2")
   
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

   ;; find-vars tests
   (check-equal? (find-vars (int 10))
                 (set) "find-vars in an int expression")

   (check-equal? (find-vars (var "x"))
                 (set (var "x")) "find-vars in a var expression")
   (check-equal? (find-vars (apair (var "x") (var "y")))
                 (set (var "x") (var "y")) "find-vars in apair expression")

   (check-equal? (find-vars (add (var "x") (var "y")))
                 (set (var "x") (var "y")) "find-vars in an add expression")
   (check-equal? (find-vars (add (int 1) (int 2)))
                 (set) "find-vars in an add expression with no vars")

   (check-equal? (find-vars (ifgreater (var "x") (var "y") (var "z") (aunit)))
                 (set (var "x") (var "y") (var "z")) "find-vars in an ifgreater expression")

   (check-equal? (find-vars (fun "foo" "x" (add (var "x") (var "y"))))
                 (set (var "y")) "find-vars in a fun expression")
   (check-equal? (find-vars (fun "foo" "x" (fun "bar" "y" (var "z"))))
                 (set (var "z")) "find-vars in a nested fun expression")

   (check-equal? (find-vars (call (fun "foo" "x" (var "y")) (int 10)))
                 (set (var "y")) "find-vars in a call expression")

   (check-equal? (find-vars (mlet "x" (int 10) (var "y")))
                 (set (var "y")) "find-vars in a mlet expression")

   (check-equal? (find-vars (fst (apair (int 1) (int 2))))
                 (set) "find-vars in a fst expression")

   (check-equal? (find-vars (snd (apair (int 1) (int 2))))
                 (set) "find-vars in a snd expression")

   (check-equal? (find-vars (aunit))
                 (set) "find-vars in aunit expression")

   (check-equal? (find-vars (isaunit (var "x")))
                 (set (var "x")) "find-vars in an isaunit expression")

   ;; compute-free-vars tests
   (check-equal? (compute-free-vars (int 1)) (int 1))
   (check-equal? (compute-free-vars (fun #f "x" (var "y")))
                 (fun-challenge #f "x" (var "y") (set (var "y"))) "compute-free-vars test")

   ;; select-env tests
   (check-equal? (select-env (list (cons "x" 10)) (set (var "x")))
                 (list (cons "x" 10)) "select-env test")
   (check-equal? (select-env (list (cons "x" 10)) (set (var "y")))
                 (list) "select-env test")

   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
(run-tests challenge-tests)
