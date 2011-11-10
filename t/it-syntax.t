#!/usr/bin/env gosh

(add-load-path ".")
(use gauche.parameter)
(use test.gasmine)

(describe "it"
  (define (unlines . ss)
    (string-join ss "\n" 'suffix))

  (it "should output its description"
    (expect
      (with-gasmine-output-to-string
        (lambda ()
          (describe "-"
            (it "should succeed"
              (expect #t eq? #t)))))
      equal?
      (unlines "ok 1 - should succeed"
               "1..1"))
    )
  (it "should output details about failure with expected value"
    (expect
      (with-gasmine-output-to-string
        (lambda ()
          (describe "-"
            (it "should fail"
              (expect '(#t) equal? '(#f))))))
      equal?
      (unlines "not ok 1 - should fail"
               "# Expected '(#t) equal? '(#f)"
               "#     Actual value: (#t)"
               "#   Expected value: (#f)"
               "1..1"))
    )
  (it "should output details about failure without expected value"
    (expect
      (with-gasmine-output-to-string
        (lambda ()
          (describe "-"
            (it "should match"
              (define (foo) "foo")
              (expect (foo) #/^bar$/)))))
      equal?
      (unlines "not ok 1 - should match"
               "# Expected (foo) #/^bar$/"
               "#     Actual value: \"foo\""
               "1..1"))
    )
  (it "should output details about evaluation failure"
    (expect
      (with-gasmine-output-to-string
        (lambda ()
          (describe "-"
            (it "should fail"
              (expect (#t) equal? '(#f))))
          (describe "-"
            (it "should fail"
              (expect '(#t) equal? (#f))))))
      equal?
      (unlines "not ok 1 - should fail"
               "# Expected (#t) equal? '(#f)"
               (format "#     Actual value: ~s"
                       (make <error> :message "invalid application: (#t)"))
               "#   Expected value: (#f)"
               "not ok 2 - should fail"
               "# Expected '(#t) equal? (#f)"
               "#     Actual value: (#t)"
               (format "#   Expected value: ~s"
                       (make <error> :message "invalid application: (#f)"))
               "1..2"))
    )
  (it "should output its description with SKIP message"
    (expect
      (with-gasmine-output-to-string
        (lambda ()
          (describe "-"
            (it "should succeed"
              (SKIP "required tools not found")))))
      equal?
      (unlines "ok 1 - # SKIP should succeed (required tools not found)"
               "1..1"))
    )
  (it "should output its description with TODO message"
    (expect
      (with-gasmine-output-to-string
        (lambda ()
          (describe "-"
            (it "should succeed"
              (TODO))
            (it "should succeed"
              (TODO "known breakage")))))
      equal?
      (unlines "not ok 1 - # TODO should succeed"
               "not ok 2 - # TODO should succeed (known breakage)"
               "1..2"))
    )
  )

(run-suites)

; vim: filetype=scheme
