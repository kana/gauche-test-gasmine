#!/usr/bin/env gosh

(add-load-path ".")
(use test.gasmine)

(define stack '())

(describe "suite 1"
  (push! stack "1")
  (it "is dummy"
    (push! stack "1-1"))
  (it "is dummy"
    (push! stack "1-2"))
  (it "is dummy"
    (push! stack "1-3")))

(describe "suite 2"
  (push! stack "2")
  (it "is dummy"
    (push! stack "2-1"))
  (it "is dummy"
    (push! stack "2-2"))
  (it "is dummy"
    (push! stack "2-3")))

(describe "suite 3"
  (push! stack "3")
  (it "is dummy"
    (push! stack "3-1"))
  (it "is dummy"
    (push! stack "3-2"))
  (it "is dummy"
    (push! stack "3-3")))

(describe "suite evaluation order"
  (it "should evaluate suites and specs in order of definition"
    (expect (reverse stack) equal? '("1" "2" "3"
                                     "1-1" "1-2" "1-3"
                                     "2-1" "2-2" "2-3"
                                     "3-1" "3-2" "3-3"))))

(run-suites)

; vim: filetype=scheme
