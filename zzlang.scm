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



(define err #f)
(let ((error-message
       (call-with-current-continuation
	(lambda (continuation)
	  (set! err (lambda args
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

(define (app f . args)
  (cond ((procedure? f)
	 (apply f args))
	()))

(define (forc ())
  (if (procedure?)
      (apply env)))

(define (def name . body)
  (cond
   ((and (pair? name)
	 (not (eq? (car name) 'quote)))
    (if (not (pair? (cdr name)))
	(err "incorrect function definition (define " ,name " ...)"))
    (if (null? (cddr name))
	;; First-class functions:
	(def (car name)
	     `(function ,(cadr name)
			,@body))
	;; Currying:
	(def (list (car name) (cadr name))
	     `(define (,(car name) ,@(cddr name))
		,@body)
	     (car name))))
   ((null? body)
    (err "no definition in (define " name ")"))
   ((not (null? (cdr body)))
    (apply err `("too many definitions in (define " ,name ,@body ")")))
   ;; Normalized.
   ((symbol? name)
    ;; Identifier.
    (cons name (car body)))
   ((and (pair?         name)
	 (eq?     ( car name) 'quote)
	 (symbol? (cadr name))
	 (null?   (cddr name)))
    ;; Wildcard.
    (cons #t (if (eq? (caar body) 'function)
		 (cons (cadr name) (cdar body))
		 (car body))))
   (#t
    (err "cannot define " name))))

pair      -> apply
boolean   -> boolean
symbol    -> lookup in closure
char      -> char
vector    -> closure
procedure -> apply when forcing
number    -> integer/rational/real/complex literal
string    -> list of char
port      -> err

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

(define (evaluate . forms)
  (let ((env (cons 'env ())))
    )
  (cond
   ((null? forms) forms)
   ((and (list? (car forms))
	 (not (null? (car forms)))
	 (eq? (caar forms) 'define))
    (apply evaluate
	   (apply def env (cdar forms))
	   (cdr forms)))
   (#t
    (car forms))))

(define (main . files)
  (define (validate form)
    (cond
     ((pair? form)
      (validate (car form))
      (validate (cdr form)))
     ((vector? form)
      (err "incorrect syntax in " form))))
  (define (slurp port)
    (if (string? port)
	(call-with-input-file port slurp)
	(let ((form (read port)))
	  (if (eof-object? form)
	      '()
	      (begin
		(validate form)
		(cons form (slurp port)))))))
  (dump
   ;; TODO ban 0-arg functions (only 1 arg): unnecessary in language where all expressions are lazy
   (app `(function ()
		   ,@library
		   ,@(apply append
			    (map slurp files))))))

;;; zzlang.scm ends here
