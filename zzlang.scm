#!/usr/local/bin/guile \
-e main -s
!#

;;; zzlang.scm --- R5RS-compliant interpreter of a subset of zlang for bootstrapping purposes.
;; Don't use for any other purpose. zzlang is unoptimized, outputs only binary, may silently ignore some invalid inputs, does not guarantee termination, and has limited error-handling capabilities.

(define library
  `((define + ,+)
    (define - ,-)
    (define * ,*)
    (define = ,=)
    (define < ,<)))



(define error #f)
(let ((error-message
       (call-with-current-continuation
	(lambda (continuation)
	  (set! error (lambda args
			(continuation args)))
	  #f))))
  (if error-message
      (begin
	(display "error: ")
	(for-each
	 (lambda (part)
	   ((if (string? part) display write) part))
	 error-message)
	(newline))))

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
	   (string->number (substring hex i (+ i 2)) 16))))))
   list))

(define (forc ())
  (if (procedure?)
      (apply environment)))

(define (function signature . body)
  ())

(define (def closure signature . body)
  (cond ((pair? signature)
	 (if (memq 'quote signature)
	     (error "incorrectly placed quote in (define " signature " ...)"))
	 (def environment (car signature)
	      (function ,(cdr signature)
			,@body)))
	((not (null? (cdr body)))
	 (apply error `("too many definitions in (define " ,signature ,@body ")")))
	((symbol? signature)
	 (set-car! closure (cons signature (car body)))
	 ;; Variable.
	 )
	((and (pair?         signature)
	      (eq?     ( car signature) 'quote)
	      (symbol? (cadr signature))
	      (null?   (cddr signature)))
	 (set-car! closure (cons #t (car body)))
	 ;; Wildcard.
	 )
	(#t
	 (error "cannot define " signature))))

pair      -> closure
boolean   -> boolean
symbol    -> lookup in closure
char      -> char
vector    -> error
procedure -> apply when forcing
number    -> integer/rational/real/complex literal
string    -> list of char
port      -> error

(env . exp)

((def1 . val1)
 (def2
   (function ((list _))
	     ())
   (#f . func2))
 (#t (wildcard-name ('a 'b)
		    ())
     (wildcard-name2  ('x)
		      x))
 . exp)

(define (app . forms)
  (let ((environment (cons 'environment ())))
    )
  (define (relabel form)
    )
  (cond ((null? forms) forms)
	((and (list? (car forms))
	      (not (null? (car forms)))
	      (eq? (caar forms) 'define))
	 (apply evaluate
		(apply def environment (cdar forms))
		(cdr forms)))
	(#t
	 (car forms))))

(define (main . files)
  (define (load port)
    (let ((form (read port)))
      (if (eof-object? form)
	  '()
	  (cons form (load port)))))
  (dump
   (evaluate
    (apply append
	   library
	   (map (lambda (file)
		  (call-with-input-file file load))
		files)))))

;;; zzlang.scm ends here
