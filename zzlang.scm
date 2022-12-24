#!/usr/local/bin/guile \
-e main -s
!#

;;; zzlang.scm --- Pure R5RS interpreter of a subset of zlang for bootstrapping purposes.

;; N.B. Some procedure names have been abbreviated to deconflict them with commonly defined procedures in Scheme implementations.


;;; Exception facility

(define (report message)
  (let print ((separate? #f)
	      (remaining message))
    (cond
     ((null? remaining)
      (newline))
     ((string? (car remaining))
      (display (car remaining))
      (print #f (cdr remaining)))
     (else
      (if separate?
	  (display " "))
      (write (car remaining))
      (print #t (cdr remaining))))))

(define err #f)
(define-syntax catch
  (syntax-rules ()
    ((catch e body handler)
     (let ((olderr err)
	   (e (call-with-current-continuation
	       (lambda (continuation)
		 (lambda args (continuation args))))))
       (if (procedure? e)
	   (dynamic-wind
	     (lambda () (set! err e))
	     (lambda () body)
	     (lambda () (set! err olderr)))
	   handler)))
    ((catch e handler)
     (let ((e (call-with-current-continuation
	       (lambda (continuation)
		 (set! err (lambda args (continuation args)))
		 #f))))
       (if e handler)))))

(catch e
  (begin
    (display "error: ")
    (report e)))


;;; Generator facility

(define done-object (list 'done)) ; Unique object signaling generator exhaustion.
(define (done-object? obj)
  (eq? obj done))
(define-syntax generator
  (syntax-rules ()
    ((generator yield args . body)
     (lambda args
       (define (state)
	 (call-with-current-continuation
	  (lambda (outer)
	    (define (yield x)
	      (call-with-values
		  (lambda ()
		    (call-with-current-continuation
		     (lambda (current)
		       (set! state current)
		       (outer x))))
		(lambda () #t)))
	    (let ((end (begin . body)))
	      (set! state (lambda () done-object))
	      end))))
       (lambda () (state))))))

(define-syntax define-generator
  (syntax-rules ()
    ((define-generator yield (name . args) . body)
     (define name
       (generator yield args . body)))))


;;; Scheme interface

(define (curry f)
  (lambda (a) (lambda (b) (f a b))))

(define (str s)
  (cons 'string (string->list s)))


;;; Data structures

(define (name? form)
  (and (vector?          form)
       (= (vector-length form) 1)))
(define (name-symbols name)
  (vector-ref name 0))
(define (env)
  (letrec-syntax
      ((get!
	(syntax-rules ()
	  ((get! name alist default)
	   (let ((pointer (assq name alist)))
	     (if pointer
		 (set! pointer (cdr pointer))
		 (begin
		   (set! pointer (list default))
		   (set! alist (cons (cons name pointer)
				     store))))
	     pointer))
	  ((get! name alist)
	   (get! name alist (list name))))))
    (define store '())
    (define aliases '())
    (define count 0)
    (define-generator yield (iterator . keys)
      (let iterate ((stacks (map (lambda (key)
				   ;; Create a new pointer to the head of each stack:
				   (list (car (get! key store '()))))
				 keys)))
	(let maximize ((stack '(; Pointer.
				(; List.
				 (-1 ; Count.
				  ;; . '() ; Value.
				  ))))
		       (remaining stacks)
		       (empty? #t))
	  (if (null? remaining)
	      (if empty?
		  (cdaar stack)
		  (begin
		    (yield (cdaar stack))
		    (set-car! stack (cdar stack))
		    (iterate stacks)))
	      (if (null? (caar remaining))
		  (maximize stack (cdr remaining) empty?)
		  (maximize (if (> (caaaar remaining) (caaar stack))
				(car remaining)
				stack)
			    (cdr remaining)
			    #f))))))
    (lambda (name . value)
      (if (null? value)
	  ;; Get:
	  (cond
	   ((name? name)
	    (apply iterator (name-symbols name)))
	   ((symbol? name)
	    ;; Get proper name:
	    (vector (car (get! name aliases))))
	   ((pair? name)
	    ;; Get proper names:
	    )
	   (else
	    (iterator name)))
	  ;; Set:
	  (if (symbol? (car value))
	      ;; Alias:
	      (if (not (eq? name (car value)))
		  (let ((a (get! name aliases))
			(b (get! (car value) aliases)))
		    (if (not (eq? a b))
			;; (car a) and (car b) are guaranteed to be disjoint at this point.
			(set-car! a (append (car a) (car b)))
			(set-car! b (car a)))))
	      ;; Push:
	      (let ((pointer (get! name store '())))
		(set-car! pointer (cons (cons count (car value))
					(car pointer)))
		(set! count (+ count 1))))))))

(define (closure store form)
  (vector store form))
(define (closure? form)
  (and (vector?          form)
       (= (vector-length form) 2)))
(define (closure-environment cl)
  (vector-ref cl 0))
(define (closure-form cl)
  (vector-ref cl 1))


;;; Read syntax predicates

(define (wildcard? form)
  (and (pair?         form)
       (eq?     ( car form) 'quote)
       (symbol? (cadr form))
       (null?   (cddr form))))

(define (function? form)
  (and (pair?      form)
       (eq?   (car form) 'function)))


;; Interpreter

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

(define (forc envs form)
  (cond
   ((and (closure? form)
	 (not (function? (closure-form form))))
    ;; Unwrap closure:
    (forc (cons (closure-environment form) envs)
	  (closure-form form)))
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
   ((not (null? (cddr form))) ; Call with multiple arguments:
    ;; Currying calls:
    (forc envs (cons (list (car form) (cadr form)) (cddr form))))
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
    ;; Force until not a vector or a pair or a symbol, then pass to procedure.
    )))

(define (def store name . body)
  (cond
   ((null? body)
    (err "no definition in (define " name ")"))
   ((and (not (pair? name))
	 (not (null? (cdr body))))
    (apply err `("too many definitions in (define " ,name ,@body ")")))
   ((symbol? name)
    (store name (car body)))
   ((wildcard? name)
    (store #t (if (function? (car body))
		  (cons (cadr name) (cdar body))
		  (car body))))
   ((not (pair? name))
    (err "cannot define " name))
   ;; Function definition:
   ((pair? (car name))
    ;; Deep definition:
    (apply def store (append (car name) (cdr name))
	   body))
   ((null? (cddr name))
    ;; First-class functions:
    (def store (car name)
	 `(function ,(cadr name)
		    ,@body)))
   (else
    ;; Currying definitions:
    (def store (list (car name) (cadr name))
	 `(define (,(car name) ,@(cddr name))
	    ,@body)
	 (car name)))))

(define (scope store . body)
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
  (apply scope (apply append (map slurp files))))

;;; zzlang.scm ends here
