#!/usr/bin/env gosh

(add-load-path ".")
(use gauche.parameter)
(use test.gasmine)

(describe "before"
  (it "should be evaluate before evaluating a spec"
    (expect
      (let1 stack '()
        (with-gasmine-output-to-null
          (lambda ()
            (describe "-"
              (before (push! stack "b1"))
              (before (push! stack "b2"))
              (before (push! stack "b3"))
              (it "i1" (push! stack "i1"))
              (it "i2" (push! stack "i2"))
              (it "i3" (push! stack "i3")))))
        (reverse stack))
      equal?
      '("b1" "b2" "b3" "i1"
        "b1" "b2" "b3" "i2"
        "b1" "b2" "b3" "i3"))
    )
  )

(describe "after"
  (it "should be evaluate after evaluating a spec"
    (expect
      (let1 stack '()
        (with-gasmine-output-to-null
          (lambda ()
            (describe "-"
              (after (push! stack "a1"))
              (after (push! stack "a2"))
              (after (push! stack "a3"))
              (it "i1" (push! stack "i1"))
              (it "i2" (push! stack "i2"))
              (it "i3" (push! stack "i3")))))
        (reverse stack))
      equal?
      '("i1" "a1" "a2" "a3"
        "i2" "a1" "a2" "a3"
        "i3" "a1" "a2" "a3"))
    )
  )

(run-suites)

; vim: filetype=scheme
