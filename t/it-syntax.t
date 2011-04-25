#!/usr/bin/env gosh

(add-load-path ".")
(use gauche.parameter)
(use gauche.test.gasmine)

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
              (it "should succeed" TODO "known breakage"
                (expect #t eq? #f)))
            (run-suites))))
      equal?
      (unlines "not ok 1 - should succeed # TODO known breakage"
               "# Expected #t eq? #f"
               "1..1"))
    )
  )

(run-suites)

; vim: filetype=scheme
