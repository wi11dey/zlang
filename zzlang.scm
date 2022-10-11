#!/usr/local/bin/guile \
-e main -s
!#

;;; zzlang.scm --- R5RS-compliant interpreter for a subset of zlang for bootstrapping purposes.
;; Do NOT use for any other purpose. zzlang is unoptimized, outputs only binary, may silently ignore some invalid inputs, does not guarantee termination, and has limited error-handling capabilities.

(define environment '())
(define invocations 0)
(define )

(define error #f)
(let ((error-message
       (call-with-current-continuation
	(lambda (cont)
	  (set! error (lambda args
			(cont args)))
	  #f))))
  (if error-message
      (display "error: ")
      (for-each
       (lambda (part)
	 (display part))
       error-message)
      (newline)))

(define (dump list)
  (for-each
   (lambda (element)
     (let ((hex (number->string element 16)))
       (if (odd? (string-length hex))
           (set! hex (string-append "0" hex)))
       (do ((i 0 (+ i 2)))
           ((= i (string-length hex)))
         (write-char
	  (integer->char
	   (string->number
	    (substring hex i (+ i 2))
	    16))))))
   list))

(define (qualify context symbol)
  (string->symbol (string-append (symbol->string context) "." (symbol->string symbol))))

(define (match? type candidate)
  (let compare ((type-chars      (string->list (symbol->string type)))
		(candidate-chars (string->list (symbol->string candidate))))
    (if (null? type-chars)
	(or (null? candidate-chars)
	    (char=? (car candidate-chars) #\.))
	(and (not (null? candidate-chars))
	     (char=? (car type-chars)
		     (car candidate-chars))
	     (compare (cdr type-chars)
		      (cdr candidate-chars))))))

(define (lower))

(define (def environment context signature . body)
  (define (parameter form)
    )
  (cond ((symbol? signature)
	 (let ((result (apply evaluate
			      environment
			      (qualify context signature)
			      body))))
	 (cons
	  (car result))
	 )
	((list? signature)
	 (if (not (symbol? (car signature)))))
	(t
	 (error signature " is not a valid identifier"))))

(define (normalize define ))

(define (evaluate context . forms)
  (define (relabel form)
    )
  (cons context
	(cond ((null? forms) forms)
	      ((and (list? (car forms))
		    (not (null? (car forms)))
		    (eq? (caar forms) 'define))
	       (apply evaluate
		      (apply def environment context
			     (cdar forms))
		      context
		      (cdr forms)))
	      (#t
	       (car forms)))))

(define (main . files)
  (define (load port)
    (let ((form (read port)))
      (if (eof-object? form)
	  '()
	  (cons form (load port)))))
  (dump
   (evaluate '() #f
	     (apply append (map (lambda (file)
				  (call-with-input-file file load))
				files)))))

;;; zzlang.scm ends here
