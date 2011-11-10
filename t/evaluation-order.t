#!/usr/bin/env gosh

(add-load-path ".")
(use gauche.parameter)
(use test.gasmine)

(define (with-output-to-null thunk)
  (let1 result #f
    (with-output-to-string
      (lambda ()
        (set! result (thunk))))
    result))

(describe "suite evaluation order"
  (it "should evaluate suites and specs in order of definition"
    (expect
      (let1 stack '()
        (with-gasmine-output-to-null
          (lambda ()
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
                (push! stack "3-3")))))
        (reverse stack))
       equal?
       '("1" "2" "3"
         "1-1" "1-2" "1-3"
         "2-1" "2-2" "2-3"
         "3-1" "3-2" "3-3"))))

(run-suites)

; vim: filetype=scheme
