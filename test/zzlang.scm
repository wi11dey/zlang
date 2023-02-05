;;; tests/zzlang.scm --- Unit tests for zzlang.

;; Load zzlang.scm (from parent directory of this file) before executing this file as a script.


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

(testset "structs"
	 (struct (test1 field1 field2))
	 (struct (test2 field3))
	 (let ((instance1 ((test1) 1 2))
	       (instance2 ((test2) 3)))
	   (assert "instance of 1" ((is? test1) instance1) = #t)
	   (assert "instance of 2" ((is? test2) instance2) = #t)
	   (assert "not instance 1" ((is? test1) instance2) = #f)
	   (assert "not instance 2" ((is? test2) instance1) = #f)
	   (assert "field access 1" ((test1 'field1) instance1) = 1)
	   (assert "field access 2" ((test1 'field2) instance1) = 2)
	   (assert "field access 3" ((test2 'field3) instance2) = 3)
	   (assert error "unknown field"
		   ((test1 'field3) instance1))
	   (assert error "invalid syntax"
		   ((test1 'field1 'field2) instance1))
	   (assert error "wrong type"
		   ((test1 'field1) instance2))))


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
