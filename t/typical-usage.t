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

(describe "custom matcher"
  (define (modulo-7? a b) (= (modulo a 7) (modulo b 7)))
  (it "should succeed if given numbers are congruent modulo 7"
    (expect 7 modulo-7? 7)
    (expect 8 modulo-7? 1)
    (expect (+ (* 7 5) 3) modulo-7? 3)
    )
  (it "should succeed if given numbers are not congruent modulo 7"
    (expect 7 not modulo-7? 8)
    (expect 8 not modulo-7? 2)
    (expect (+ (* 7 5) 3) not modulo-7? 4)
    )
  )

(run-suites)

; vim: filetype=scheme
