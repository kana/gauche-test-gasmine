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

(describe "with-output-to-null"
  (it "should discard output to the current port"
    (expect
      (with-output-to-null
        (lambda ()
          (write "hi")
          'hey))
      eq?
      'hey)
    )
  )

(describe "before"
  (it "should be evaluate before evaluating a spec"
    (expect
      (parameterize ([all-suites '()])
        (let1 stack '()
          (describe "-"
            (before (push! stack "b1"))
            (before (push! stack "b2"))
            (before (push! stack "b3"))
            (it "i1" (push! stack "i1"))
            (it "i2" (push! stack "i2"))
            (it "i3" (push! stack "i3"))
            )
          (with-output-to-null
            run-suites)
          (reverse stack)))
      equal?
      '("b1" "b2" "b3" "i1"
        "b1" "b2" "b3" "i2"
        "b1" "b2" "b3" "i3"))
    )
  )

(describe "after"
  (it "should be evaluate after evaluating a spec"
    (expect
      (parameterize ([all-suites '()])
        (let1 stack '()
          (describe "-"
            (after (push! stack "a1"))
            (after (push! stack "a2"))
            (after (push! stack "a3"))
            (it "i1" (push! stack "i1"))
            (it "i2" (push! stack "i2"))
            (it "i3" (push! stack "i3"))
            )
          (with-output-to-null
            run-suites)
          (reverse stack)))
      equal?
      '("i1" "a1" "a2" "a3"
        "i2" "a1" "a2" "a3"
        "i3" "a1" "a2" "a3"))
    )
  )

(run-suites)

; vim: filetype=scheme
