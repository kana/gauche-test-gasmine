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
  (it "should not affect identifier ``not'' in actual-value"
    (expect (not #f) eq? #t)
    (expect (not #t) eq? #f)
    )
  (it "should not affect identifier ``not'' in expected-value"
    (expect #t eq? (not #f))
    (expect #f eq? (not #t))
    )
  (it "should not affect identifier ``not'' that is not a matcher prefix"
    (expect not eq? not)
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

(describe "expect without expected value"
  (it "should succeed if (given-predicate? actual-value) returns true"
    (expect '() null?)
    (expect (list) null?)
    (expect '(hi) not null?)
    (expect (list 'hi) not null?)
    )
  )

(describe "expect with error"
  (it "should succeed if evaluation of actual-value raises an error"
    (expect (car '()) raise?)
    (expect (with-input-from-string "\"" read) raise?)

    (expect (car '(a . b)) not raise?)
    (expect (with-input-from-string "(a . b)" read) not raise?)
    )
  (it "should succeed if a specified class of error is raised"
    (expect (car '()) raise? <error>)
    (expect (car '()) not raise? <read-error>)

    (expect (with-input-from-string "\"" read) raise? <error>)
    (expect (with-input-from-string "\"" read) raise? <read-error>)
    (expect (with-input-from-string "\"" read) not raise? <system-error>)
    )
  (it "should identify evaluation failure" TODO "known breakage"
    (define <error>-instance (make <error>))
    (expect <error>-instance not raise? <error>)
    )
  )

(run-suites)

; vim: filetype=scheme
