#!/usr/bin/env gosh

(add-load-path ".")
(use gauche.parameter)
(use test.gasmine)

(describe "it"
  (define (unlines . ss)
    (string-join ss "\n" 'suffix))

  (it "should output its description"
    (expect
      (with-output-to-string
        (lambda ()
          (parameterize ([all-suites '()])
            (describe "-"
              (it "should succeed"
                (expect #t eq? #t)))
            (run-suites))))
      equal?
      (unlines "ok 1 - should succeed"
               "1..1"))
    )
  (it "should output details about failure"
    (expect
      (with-output-to-string
        (lambda ()
          (parameterize ([all-suites '()])
            (describe "-"
              (it "should fail"
                (expect '(#t) equal? '(#f))))
            (run-suites))))
      equal?
      (unlines "not ok 1 - should fail"
               "# Expected (#t) equal? (#f)"
               "1..1"))
    )
  (it "should output its description with SKIP message"
    (expect
      (with-output-to-string
        (lambda ()
          (parameterize ([all-suites '()])
            (describe "-"
              (it "should succeed" SKIP "required tools not found"
                (expect #t eq? #t)))
            (run-suites))))
      equal?
      (unlines "ok 1 - should succeed # SKIP required tools not found"
               "1..1"))
    )
  (it "should output its description with TODO message"
    (expect
      (with-output-to-string
        (lambda ()
          (parameterize ([all-suites '()])
            (describe "-"
              (it "should succeed" TODO)
              (it "should succeed" TODO "known breakage"))
            (run-suites))))
      equal?
      (unlines "not ok 1 - # TODO should succeed"
               "not ok 2 - # TODO should succeed (known breakage)"
               "1..2"))
    )
  )

(run-suites)

; vim: filetype=scheme
