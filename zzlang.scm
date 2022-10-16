#!/usr/local/bin/guile \
-e main -s
!#

;;; zzlang.scm --- Pure R5RS interpreter of a subset of zlang for bootstrapping purposes.
;; Don't use for any other purpose. zzlang is unoptimized, outputs only binary, may silently ignore some invalid inputs, does not guarantee termination, and has limited error-handling capabilities.

(define (str s)
  `(string ,@(string->list s)))



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
	(let report ((separate #f)
		     (remaining error-message))
	  (if (string? (car remaining))
	      (begin
		(display (car remaining))
		(report #f (cdr remaining)))
	      (begin
		(if separate
		    (display " "))
		(write (car remaining))
		(report #t (cdr remaining)))))
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

(define (wildcard? form)
  (and (pair?         form)
       (eq?     ( car form) 'quote)
       (symbol? (cadr form))
       (null?   (cddr form))))

(define (match? pattern form)
  (cond
   ((wildcard? pattern)
    (list (cons (cadr pattern) form)))
   ((pair? pattern)
    (cond
     ((symbol? (car pattern))
      (and (pair? form)
	   (eq? (car pattern) (car form))))
     ((wildcard? (car pattern)))))
   ((string? pattern)
    ;; TODO change?
    (err "invalid pattern \"" pattern "\""))
   (else
    (eqv? pattern form))))

(define (forc env form)
  (cond
   ((vector? form)
    (forc (append (vector-ref form 1) env)
	  (vector-ref form 2)))
   ((symbol? form)
    (or (assq env form)
	(assq env #t)))
   ((pair? form)
    (let app ((f    (car form))
	      (args (cdr form))))
    (cond
     ((procedure? (car form))
      (apply (car form)
	     (map
	      (lambda (object)
		(do ((native object (forc env object)))
		    ((or (boolean?   native)
			 (char?      native)
			 (procedure? native)
			 (number?    native)
			 (port?      native))
		     native)))
	      (cdr form))))
     ((not (pair? (cdr form)))
      (err "incorrect function call " form))
     ((not (null? (cddr form)))
      (forc env
	    ;; Currying calls:
	    (let ((reversed (reverse form)))
	      (list (reverse (cdr reversed))
		    (car reversed)))))
     ((and (pair? (car form))
	   (eq? (caar form) 'function)))
     ((symbol? (car form))
      (forc env form))))
   (else
    form)))

(define (def name . body)
  (cond
   ((symbol? name)
    (cons name (car body)))
   ((wildcard? name)
    (cons #t (if ((and (pair? (car body)))
		  (eq? (caar body) 'function))
		 (cons (cadr name) (cdar body))
		 (car body))))
   ((pair? name)
    (if (not (pair? (cdr name)))
	(err "incorrect function definition (define " name " ...)"))
    (if (null? (cddr name))
	;; First-class functions:
	(def (car name)
	     `(function ,(cadr name)
			,@body))
	;; Currying definitions:
	(def (list (car name) (cadr name))
	     `(define (,(car name) ,@(cddr name))
		,@body)
	     (car name))))
   ((null? body)
    (err "no definition in (define " name ")"))
   ((not (null? (cdr body)))
    (apply err `("too many definitions in (define " ,name ,@body ")")))
   (else
    (err "cannot define " name))))

(define (closure env . body)
  (cond
   ((and (pair? (car body))
	 (eq? (caar body) 'define))
    (closure (let ((pair (apply def (cdar body))))
	       (if (eq? (car pair) #t)
		   (append env (list pair))
		   (cons pair env)))
	     (cdr body)))
   ((not (null? (cdr body)))
    (apply err `("extraneous forms " ,@(cdr body) " after body")))
   (else
    (vector env (car body)))))

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
  (dump (apply closure '() (apply append (map slurp files)))))

;;; zzlang.scm ends here
