#!/usr/local/bin/guile \
-e main -s
!#

;;; zzlang.scm --- Pure R5RS interpreter of a subset of zlang for bootstrapping purposes.

;; N.B. Some procedure names have been abbreviated to deconflict them with commonly defined procedures in Scheme implementations.


;;; Exception facility

(define (report message)
  (let print ((separate #f)
	      (remaining message))
    (if (string? (car remaining))
	(begin
	  (display (car remaining))
	  (print #f (cdr remaining)))
	(begin
	  (if separate
	      (display " "))
	  (write (car remaining))
	  (print #t (cdr remaining)))))
  (newline))

(define err #f)
(define-syntax catch
  (let-syntax
      ((checkpoint
	(syntax-rules ()
	  ((checkpoint e body handler)
	   (let ((e (call-with-current-continuation
		     (lambda (continuation)
		       (set! err (lambda args
				   (continuation args)))
		       #f))))
	     (if e handler body))))))
    (syntax-rules ()
      ((catch e body handler)
       (let ((olderr err))
	 (checkpoint e
		     body
		     (begin
		       (set! err olderr)
		       handler))
	 (set! err olderr)))
      ((catch e handler)
       (checkpoint e (begin) handler)))))

(catch e
  (begin
    (display "error: ")
    (report error-message)))


;;; Scheme interface

(define (curry f)
  (lambda (a) (lambda (b) (f a b))))

(define (str s)
  (cons 'string (string->list s)))


;;; Read syntax predicates

(define (wildcard? form)
  (and (pair?         form)
       (eq?     ( car form) 'quote)
       (symbol? (cadr form))
       (null?   (cddr form))))

(define (function? form)
  (and (pair?      form)
       (eq?   (car form) 'function)))


;;; Record types

(define (name? form)
  (and (vector? form)
       (= (vector-length) 1)))

(define (closure store form)
  (vector store form))

(define (closure? form)
  (and (vector? form)
       (= (vector-length) 2)))

(define (closure-environment cl)
  (vector-ref cl 1))

(define (closure-form cl)
  (vector-ref cl 2))


;;; Data structures

(define (env)
  (let ((store '())
	(count 0))
    (define (new)
      (cons '() ; Values
	    '() ; Counts
	    ))
    (define (get name)
      (let ((pair (assq name store)))
	(if pair
	    (set! pair (cdr pair))
	    (begin
	      (set! pair (new))
	      (set! store (cons (cons name pair)
				store))))
	pair))
    (lambda (name . value)
      (if (null? value)
	  ;; Get:
	  (car (get name))
	  ;; Set:
	  (if (symbol? (car value))
	      ;; TODO test merge and alias:
	      (let merge ((a (get name))
			  (b (get (car value)))
			  (merged (new)))
		(cond
		 ((and (null? (car a))
		       (null? (car b)))
		  ;; Alias:
		  (set-car! a (reverse (car merged)))
		  (set-cdr! a (reverse (cdr merged)))
		  (set-car! b (car a))
		  (set-cdr! b (cdr a)))
		 ((null? (car a))
		  (set-car! merged (append (reverse (car b)) (car merged)))
		  (set-cdr! merged (append (reverse (cdr b)) (cdr merged)))
		  (set-car! b '())
		  (set-cdr! b '())
		  (merge a b merged))
		 ((null? (car b))
		  (merge b a merged))
		 (else
		  (if (>= (cadr a)
			  (cadr b))
		      (begin
			(set-car! merged (cons (caar a) (car merged)))
			(set-cdr! merged (cons (cadr a) (cdr merged)))
			(set-car! a (cdar a))
			(set-cdr! a (cddr a))
			(merge a b merged))
		      (merge b a merged)))))
	      ;; Push:
	      (let ((pair (get name)))
		(set-car! pair (append value (car pair)))
		(set-cdr! pair (cons count   (cdr pair)))
		(set! count (+ count 1))))))))




(define (app to signature . body)
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
    (err "invalid pattern \"" pattern "\"")) ; Change?
   (else
    (eqv? pattern form))))

;; TODO consider procedures thoroughly
(define (forc env form)
  (cond
   ((and (vector? form)
	 (not (function? (vector-ref form 2))))
    ;; Unwrap closure:
    (forc (append (vector-ref form 1) env)
	  (vector-ref form 2)))
   ((symbol? form)
    (assq form env)) ; Wildcards only work for function calls.
   ((string? form)
    (forc env (str form)))
   ((not (pair? form))
    form)
   ;; Function call:
   ((eq? (car form) 'quote)
    (err "incorrect quotation " form))
   ((not (pair? (cdr form)))
    (err "incorrect function call " form))
   ((not (null? (cddr form)))
    ;; Call with multiple arguments:
    (forc env
	  ;; Currying calls:
	  (let ((reversed (reverse form)))
	    (list (reverse (cdr reversed))
		  (car reversed)))))
   ;; Normalized.
   ((function? form)
    ;; Anonymous function:
    (vector env form))
   ((function? (car form))
    ;; Anonymous function call:
    (apply app (cadr form) (cdar form)))
   ((pair? (car form))
    ;; Curried call:
    (forc env (cons (forc env (car form))
		    (cdr form))))
   ((vector? (car form))
    ;; First, enter closure of function:
    (forc env (vector (vector-ref 1 (car form))
		      (cons (vector-ref 2 (car form))
			    (cdr form)))))
   ((vector? (cadr form))
    ;; Then, enter closure of argument:
    (forc env (vector (vector-ref 1 (cadr form))
		      (list (car form)
			    (vector-ref 2 (cadr form))))))
   ;; ((symbol? (car form))
   ;;  find all functions in environment that match
   ;;  (for-each function that matches
   ;; 	      )
   
   ;;  collect all function definitions since last value definition of (car form)
   ;;  (if empty set
   ;; 	force last value definition and recurse
   ;; 	)
   
   ;;  (if last definition in env of (car form) was function
   ;; 	find all the definitions of (car form) up to and not including the last non-function definition
   ;; 	force the value and recurse)
   ;;  (assq (car form) env)
   ;;  (let ((forced (forc env (cadr form))))
   ;;    (if (eq? forced (cadr form))
   ;; 	  (if (procedure? (car form))
   ;; 	      (apply (car form) forced)
   ;; 	      ;; No match.
   ;; 	      )))
   ;;  )
   ((procedure? (car form))
    )))

(define (def store name . body)
  (cond
   ((null? body)
    (err "no definition in (define " name ")"))
   ((not (null? (cdr body)))
    (apply err `("too many definitions in (define " ,name ,@body ")")))
   ;; Validated body.
   ((symbol? name)
    (cons (cons name (car body))
	  env))
   ((wildcard? name)
    (append env
	    (list
	     (cons #t (if (function? (car body))
			  (cons (cadr name) (cdar body))
			  (car body))))))
   ((pair? name)
    (if (not (pair? (cdr name)))
	(err "incorrect function definition (define " name " ...)"))
    (if (null? (cddr name))
	;; First-class functions:
	(def env (car name)
	     `(function ,(cadr name)
			,@body))
	;; Currying definitions:
	(def env (list (car name) (cadr name))
	     `(define (,(car name) ,@(cddr name))
		,@body)
	     (car name))))
   (else
    (err "cannot define " name))))

(define (closure store . body)
  (cond
   ((and (pair? (car body))
	 (eq? (caar body) 'define))
    (apply def store (cdar body))
    (closure store (cdr body)))
   ((not (null? (cdr body)))
    (apply err `("extraneous forms " ,@(cdr body) " after body")))
   (else
    (vector (list store) (car body)))))


;;; Entry point

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
  (apply closure (apply append (map slurp files))))

;;; zzlang.scm ends here
