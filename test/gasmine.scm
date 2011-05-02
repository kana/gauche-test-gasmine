(define-module test.gasmine
  (export
    ; Public API
    after
    before
    describe
    expect
    it
    raise?
    run-suites

    ; Not public, but exported to test.
    all-suites
    ))
(select-module test.gasmine)

(use gauche.parameter)
(use util.list)
(use util.match)




;;; Matchers

; Currently ordinary predicates can be used as matchers.

(define (raise? value :optional (error-class <error>))
  (and
    (is-a? value <evaluation-failure>)
    (is-a? (slot-ref value 'original-error) error-class)))




;;; Expectations

; TODO: "Bail out!" syntax.

(define-class <expectation-failure> (<message-condition>)
  ())

(define (stop-running-this-spec message)
  (error <expectation-failure>
         :message message))

(define-constant %absent-value (list '%absent-value))

(define-class <evaluation-failure> ()
  ([original-error
     :init-keyword :original-error]))

(define-syntax eval-with-error-trap
  (syntax-rules ()
    [(_ form)
     (guard (e [else (make <evaluation-failure> :original-error e)])
       form)]))

(define-syntax expect
  (syntax-rules (not)
    [(_ "internal" actual-value matcher-name matcher-procedure expected-value)
     (let* ([a (eval-with-error-trap actual-value)]
            [e expected-value]  ; ... should be evaluated without error.
            [arguments (if (eq? e %absent-value)
                         (list a)
                         (list a e))])
       (if (apply matcher-procedure arguments)
         #t
         (stop-running-this-spec
           (format "Expected ~s ~a ~s" a matcher-name e))))]
    [(_ actual-value not matcher expected-value)
     (let ([matcher-name (string-append "not" " " (symbol->string 'matcher))]
           [matcher-procedure (complement matcher)])
       (expect "internal"
               actual-value
               matcher-name
               matcher-procedure
               expected-value))]
    [(_ actual-value not matcher)
     (expect actual-value not matcher %absent-value)]
    [(_ actual-value matcher expected-value)
     (expect "internal" actual-value 'matcher matcher expected-value)]
    [(_ actual-value matcher)
     (expect actual-value matcher %absent-value)]
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
  (syntax-rules (SKIP TODO)
    [(_ description SKIP message body ...)
     (it (format "~a # SKIP ~a" description message)
         body
         ...)]
    [(_ description TODO message body ...)
     (it (format "~a # TODO ~a" description message)
         body
         ...)]
    [(_ description body ...)
     (slot-push! (current-suite)
                 'specs
                 (make <spec>
                       :description description
                       :procedure (lambda () body ...)))]))

(define-syntax before
  (syntax-rules ()
    [(_ body ...)
     (slot-push! (current-suite)
                 'before-blocks
                 (lambda () body ...))]))

(define-syntax after
  (syntax-rules ()
    [(_ body ...)
     (slot-push! (current-suite)
                 'after-blocks
                 (lambda () body ...))]))




;;; Suites

(define-class <suite> ()
  ([after-blocks
     :init-keyword :after-blocks
     :init-form (list)]  ; (thunk ...)
   [before-blocks
     :init-keyword :before-blocks
     :init-form (list)]
   [description
     :init-keyword :description
     :init-form ""]
   [ordered-specs
     :allocation :virtual
     :slot-ref (lambda (this) (reverse (slot-ref this 'specs)))]
   [specs
     :init-keyword :specs
     :init-form (list)]
   ; TODO: Nested suites.
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

(define (run-spec spec test-count parent-suite)
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
    (define (run-blocks suite type)
      (for-each
        (lambda (block) (block))
        (reverse (slot-ref parent-suite type))))
    (dynamic-wind
      (lambda ()
        (run-blocks parent-suite 'before-blocks))
      (lambda ()
        (guard (e [(<expectation-failure> e)
                   (list #f (slot-ref e 'message))])
          ((slot-ref spec 'procedure))
          (list #t #f)))
      (lambda ()
        (run-blocks parent-suite 'after-blocks))
      ))
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
          (run-spec (car rest-specs) test-count suite)
          (loop (cdr rest-specs) (+ test-count 1)))
        test-count))))

(define (run-suites :optional (x '()))
  (match x
    [(? null? x)
     (run-suites (reverse (all-suites)))]
    [(? string? path-to-spec-file)
     (run-suites (open-input-file path-to-spec-file))]
    [(? port? port)
     (parameterize ([all-suites '()])
       (let1 sandbox-module (make-module #f)
         (port-for-each (cut eval <> sandbox-module) (lambda () (read port)))
         (run-suites (reverse (all-suites)))))]
    [(and ((? suite? suite) ...) suites)
     (let ([next-test-count
             (let loop ([rest-suites suites]
                        [test-count 1])
               (if (not (null? rest-suites))
                 (loop (cdr rest-suites)
                       (run-suite (car rest-suites) test-count))
                 test-count))])
       (format #t
               "1..~a\n"
               (- next-test-count 1)))]))




(provide "test/gasmine")
