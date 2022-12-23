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
       (catch e
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
     (display title)
     (display ":\t")
     (let ((ex expected))
       (catch e
	 (let ((res result))
	   (if (equal res ex)
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
	   (report e)))))
    ((assert error title result)
     (display title)
     (display ":\t")
     (catch e
       (begin
	 result
	 (set! failed (+ failed 1))
	 (display "failed: ")
	 (write 'result)
	 (display " did not produce an error"))
       (begin
	 (set! passed (+ passed 1))
	 (display "passed")))
     (newline))))


;;; Tests

(testset "environments"
	 (define store (env))

	 (assert (store 'a) = '())

	 (store 'a 1))


;;; Summary

(newline)
(display passed)
(display " test")
(if (not (= passed 1)) (display "s"))
(display " passed")
(if (> failed 0)
    (display ", ")
    (display failed)
    (display " test")
    (if (not (= failed 1)) (display "s"))
    (display " failed"))
(if (> errors 0)
    (display ", ")
    (display errors)
    (display " uncaught error")
    (if (not (= errors 1)) (display "s")))
(display ".")
(newline)

;;; tests/zzlang.scm ends here
