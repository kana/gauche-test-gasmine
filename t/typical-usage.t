#!/usr/bin/env gosh

(add-load-path ".")
(use gauche.test.gasmine)

(describe "eq?"
  (define x 'a)
  (it "should return #t for the same objects"
    (expect (eq? 'a 'a) to-be #t)
    (expect (eq? x 'a) to-be #t)
    )
  (it "should return #f for different objects"
    (expect (eq? 'a 'b) to-be #f)
    (expect (eq? x 'b) to-be #f)
    )
  )

(run-suites)

; vim: filetype=scheme
