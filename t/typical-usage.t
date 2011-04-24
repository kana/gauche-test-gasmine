#!/usr/bin/env gosh

(add-load-path ".")
(use gauche.test.gasmine)

(describe "eq?"
  (define x 'a)
  (it "should return #t for the same objects"
    (expect (eq? 'a 'a) eq? #t)
    (expect (eq? x 'a) eq? #t)
    )
  (it "should return #f for different objects"
    (expect (eq? 'a 'b) eq? #f)
    (expect (eq? x 'b) eq? #f)
    )
  )

(describe "not eq?"
  (define x 'a)
  (it "should return #f for the same objects"
    (expect (eq? 'a 'a) not eq? #f)
    (expect (eq? x 'a) not eq? #f)
    )
  (it "should return #t for different objects"
    (expect (eq? 'a 'b) not eq? #t)
    (expect (eq? x 'b) not eq? #t)
    )
  )

(run-suites)

; vim: filetype=scheme
