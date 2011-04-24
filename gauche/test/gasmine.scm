(define-module gauche.test.gasmine
  (export
    describe
    expect
    it
    run-suites
    ))
(select-module gauche.test.gasmine)

(use gauche.parameter)
(use util.list)
(use util.match)




;;; Matchers

; Currently ordinary predicates can be used as matchers.




;;; Expectations

(define-class <expectation-failure> (<message-condition>)
  ())

(define (stop-running-this-spec message)
  (error <expectation-failure>
         :message message))

(define-syntax expect
  (syntax-rules ()
    [(_ "internal" actual-value matcher-name matcher-procedure expected-value)
     (let* ([a actual-value]
            [e expected-value])
       (if (matcher-procedure a e)
         #t
         (stop-running-this-spec
           (format "Expected ~s ~a ~s" a matcher-name e))))]
    [(_ actual-value matcher expected-value)
     (expect "internal" actual-value 'matcher matcher expected-value)]
    ; TODO: Support "not" matcher variant.
    ))




;;; Specs

(define-class <spec> ()
  ([description
     :init-keyword :description
     :init-form ""]
   [procedure
     :init-keyword :procedure
     :init-form (lambda () "NOP")]
   ))

(define-syntax it
  (syntax-rules ()
    [(_ description body ...)
     (slot-push! (current-suite)
                 'specs
                 (make <spec>
                       :description description
                       :procedure (lambda () body ...)))]))




;;; Suites

(define-class <suite> ()
  ([description
     :init-keyword :description
     :init-form ""]
   [ordered-specs
     :allocation :virtual
     :slot-ref (lambda (this) (reverse (slot-ref this 'specs)))]
   [specs
     :init-keyword :specs
     :init-form (list)]
   ; TODO: Nested suites.
   ; TODO: Before/After blocks.
   ))

(define all-suites
  (make-parameter '()))

(define current-suite
  (make-parameter
    (make <suite>
          :description "Dummy suite to avoid null check")))

(define-syntax describe
  (syntax-rules ()
    [(_ description body ...)
     (let1 new-suite (make <suite> :description description)
       (all-suites (cons new-suite (all-suites)))
       (parameterize ([current-suite new-suite])
         body ...))]))

(define (suite? x)
  (is-a? x <suite>))




;;; Utilities

(define (run-spec spec test-count)
  (define (ok test-count description additional-message)
    (format #t
            "~a ~a - ~a\n"
            "ok"
            test-count
            description))
  (define (not-ok test-count description additional-message)
    (format #t
            "~a ~a - ~a\n# ~a\n"
            "not ok"
            test-count
            description
            additional-message))
  (define (run-spec-procedure spec)
    (guard (e [(<expectation-failure> e)
               (list #f (slot-ref e 'message))])
      ((slot-ref spec 'procedure))
      (list #t #f)))
  (match-let1 (succeeded message)
              (run-spec-procedure spec)
    ((if succeeded ok not-ok)
     test-count
     (slot-ref spec 'description)
     message)))

(define (run-suite suite test-count)
  (let1 ordered-specs (slot-ref suite 'ordered-specs)
    (let loop ([rest-specs ordered-specs]
               [test-count test-count])
      (if (not (null? rest-specs))
        (begin
          (run-spec (car rest-specs) test-count)
          (loop (cdr rest-specs) (+ test-count 1)))
        test-count))))

(define (run-suites :optional (x '()))
  (match x
    [(? null? x)
     (run-suites (all-suites))]
    [(? string? path-to-spec-file)
     (run-suites (open-input-file path-to-spec-file))]
    [(? port? port)
     (parameterize ([all-suites '()])
       (let1 sandbox-module (make-module #f)
         (port-for-each (cut eval <> sandbox-module) (lambda () (read port)))
         (run-suites (all-suites))))]
    [(and ((? suite? suite) ...) suites)
     (parameterize ([all-suites suites])
       (let ([next-test-count
               (let loop ([rest-suites (all-suites)]
                          [test-count 1])
                 (if (not (null? rest-suites))
                   (loop (cdr rest-suites)
                         (run-suite (car rest-suites) test-count))
                   test-count))])
         (format #t
                 "1..~a\n"
                 (- next-test-count 1))))]))




(provide "gauche/test/gasmine")
