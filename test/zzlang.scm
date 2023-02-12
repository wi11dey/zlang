;;; tests/zzlang.scm --- Unit tests for zzlang.

;; Load zzlang.scm (from parent directory of this file) before executing this file as a script.

(define-syntax try ; Depends on `err' and `checkpoint' from zzlang.scm.
  (syntax-rules ()
    ((try e body handler)
     (let* ((olderr err)
            (e (checkpoint)))
       (if e
           (begin
             (set! err olderr)
             handler)
           (let ((result (let () body) ; Allow internal definitions in body.
                         ))
             (set! err olderr)
             result))))))


;;; Unit testing facility

(define passed 0)
(define failed 0)
(define errors 0)

(define-syntax testset
  (syntax-rules ()
    ((testset name . body)
     (begin
       (display "Testing ")
       (display name)
       (display "...")
       (newline)
       (try e
	    (begin . body)
	    (begin
	      (set! errors (+ errors 1))
	      (display "uncaught error: ")
	      (report e)
	      (display "Test set aborted.")
	      (newline)))
       (newline)))))

(define-syntax assert
  (syntax-rules (error =)
    ((assert title result = expected)
     (begin
       (display title)
       (display ":\t")
       (let ((ex expected))
	 (try e
	      (let ((res result))
		(if (equal? res ex)
		    (begin
		      (set! passed (+ passed 1))
		      (display "passed"))
		    (begin
		      (set! failed (+ failed 1))
		      (display "failed: expected ")
		      (write ex)
		      (display ", got ")
		      (write res)))
		(newline))
	      (begin
		(set! failed (+ failed 1))
		(display "failed with error: ")
		(report e))))))
    ((assert error title result)
     (begin
       (display title)
       (display ":\t")
       (try e
	    (begin
	      result
	      (set! failed (+ failed 1))
	      (display "failed: ")
	      (write 'result)
	      (display " did not produce an error"))
	    (begin
	      (set! passed (+ passed 1))
	      (display "passed")))
       (newline)))))


;;; Tests

(testset "error handling"
	 (assert error "basic" (err "test error"))
	 (assert "error"
		 (try e
		      (err "test error")
		      'ignore) = 'ignore)
	 (assert "no error"
		 (try e
		      'ignore
		      (err "test error")) = 'ignore)
	 (assert error "handler"
		 (try e
		      (err "test error")
		      (err "handler error"))))

(testset "generators"
	 (define-generator (testgen a b c) yield
	   (yield a)
	   (yield b)
	   c)
	 (let ((gen (testgen 1 2 3)))
	   (assert "iteration 1" (gen) = 1)
	   (assert "iteration 2" (gen) = 2)
	   ;; Only yielded values are returned:
	   (assert "exhaustion" (done-object? (gen)) = #t)
	   (assert "idempotent" (done-object? (gen)) = #t))
	 (let ((gen (testgen 1 2 3)))
	   (gen) ; Consume 1.
	   (for element in gen
		(assert "for syntax" element = 2)))
	 (for element in (list->generator '(1))
	      (assert "list convert" element = 1))
	 (for element in '(2)
	      (assert "implicit list" element = 2)))


;;; Summary

(display passed)
(display " test")
(if (not (= passed 1)) (display "s"))
(display " passed")
(if (> failed 0)
    (begin
      (display ", ")
      (display failed)
      (display " test")
      (if (not (= failed 1)) (display "s"))
      (display " failed")))
(if (> errors 0)
    (begin
      (display ", ")
      (display errors)
      (display " uncaught error")
      (if (not (= errors 1)) (display "s"))))
(display ".")
(newline)

;;; tests/zzlang.scm ends here
