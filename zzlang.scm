#!/usr/local/bin/guile \
-e main -s
!#

;;; zzlang.scm --- Pure R5RS interpreter of a subset of zlang for bootstrapping purposes.

;; N.B. Some procedure names have been abbreviated to deconflict them with commonly defined procedures in Scheme implementations.


;;; Exception utility

(define (report message)
  (let print ((separate? #f)
              (rest message))
    (cond
     ((null? rest)
      (newline))
     ((string? (car rest))
      (display (car rest))
      (print #f (cdr rest)))
     (else
      (if separate?
          (display " "))
      (write (car rest))
      (print #t (cdr rest))))))

(define err #f)
(define (checkpoint)
  (call-with-current-continuation
   (lambda (continuation)
     (set! err (lambda args (continuation args)))
     #f)))
;; Global fallback handler:
(let ((e (checkpoint)))
  (if e
      (begin
        (display "error: ")
        (report e))))
;; For local handlers:
(define-syntax try
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


;;; Generator utility

;; Based off of SRFI 158 `make-coroutine-generator':
(define done (list 'done)) ; Unique object signaling generator exhaustion.
(define (done-object) done) ; Consistency with R7RS style.
(define (done-object? obj)
  (or (eq? obj done)
      (eof-object? obj) ; read is also a generator.
      ))

(define-syntax generator
  (syntax-rules ()
    ((generator args yield body ...)
     (lambda args
       (define return #f)
       (define resume #f)
       (define (yield x)
         (call-with-current-continuation
          (lambda (current)
            (set! resume current)
            (return x))))
       (lambda ()
         (call-with-current-continuation
          (lambda (caller)
            (set! return caller)
            (if resume
                (resume #t)
                (begin
                  body ...
                  (set! resume (lambda (_) (return (done-object))))
                  (return (done-object)))))))))))

(define-syntax define-generator
  (syntax-rules ()
    ((define-generator (name . args) yield . body)
     (define name
       (generator args yield
                  . body)))))

(define-generator (list->generator l) yield
  (do ((current l (cdr current)))
      ((null? current))
    (yield (car current))))

(define (generator->list g)
  (let ((value (g)))
    (if (done-object? value)
	'()
	(cons (g) (generator->list g)))))

(define-syntax for
  (syntax-rules (in)
    ((for element in g . body)
     (let* ((cached g)
	    (gen (if (list? cached)
		     (list->generator g)
		     cached)))
       (do ((element (gen) (gen)))
	   ((done-object? element))
	 . body)))))


(define (special? type form) ; For special forms like quote and unquote.
  (and (pair?         form)
       (eq?     ( car form) type)
       (pair    ( cdr form))
       (null?   (cddr form))
       (symbol? (cadr form))))

(define (env . parents)
  (define store '())
  (define-generator (get key) yield
    (define exported
      (call-with-current-continuation
       (lambda (return)
	 (for entry in store
	      (cond
	       ((and (special? 'unquote (car entry))
		     (eq? (cadar entry) key))
		;; Locally defined (indicated by unquote).
		(yield entry)
		;; Redact local name when lookup proceeds to outer scopes:
		(return #t))
	       ((eq? (car entry) key)
		(yield entry))))
	 ;; Keep name when lookup proceeds to outer scopes otherwise:
	 key)))
    ;; Wildcards:
    (for entry in store
	 (if (special? 'quote (car entry))
	     (yield entry)))
    ;; Check parents:
    (if (pair? parents)
	(begin
	  ;; Primary.
	  (for entry in ((car parents) exported)
	       (yield entry))
	  (if (and (pair?    ( cdr parents))
		   (not (eq? ( car parents)
			     (cadr parents))))
	      ;; Secondary.
	      (for entry in ((cadr parents) exported)
		   (if (not (special? 'unquote (car entry)))
		       (yield entry)))))))
  (define (self . args)
    (cond
     ((null? args)
      (and (pair? parents)
	   (car parents)))
     ((null? (cdr args))
      (get (car args)))
     ((null? (cddr args))
      (set! store (cons (cons (car args) (cadr args))
			store))
      ;; Allow for chaining:
      self)))
  self)

(define (closure? cl)
  (and (vector?          cl)
       (= (vector-length cl) 2)))
(define (closure environ form)
  (if (closure? form)
      form ; Closures are idempotent.
      (vector environ form)))
(define (closure-environment cl) (vector-ref cl 0))
(define (closure-form cl)        (vector-ref cl 1))

(define (def scope name . body)
  (cond
   ((null? body)
    (err "no definition in (define " name ")"))
   ((pair? name)
    ;; Function definition:
    (cond
     ((pair? (car name))
      ;; Deep definition:
      (apply def scope (append (car name) (cdr name))
             body))
     ((null? (cddr name))
      ;; First-class functions:
      (def scope (car name)
           `(function ,(cadr name)
                      ,@body)))
     (else
      ;; Currying definitions:
      (def scope (list (car name) (cadr name))
           `(define (,(car name) ,@(cddr name))
              ,@body)
           (car name)))))
   ((not (null? (cdr body)))
    (apply err `("too many definitions in (define " ,name ,@body ")")))
   ((or (symbol? name)
	;; Wildcard:
        (special? 'quote name))
    (if (symbol? (car body))
	;; Courtesy alias:
	(scope (car body) (closure scope name)))
    (scope name (closure scope (car body))))
   (else
    (err "cannot define " name))))

(define (definition? form)
  (and (pair?      form)
       (eq?   (car form) 'define)))

(define (block scope . body)
  (cond
   ((null? body)
    (err "no expression in block"))
   ((definition? (car body))
    (apply def scope (cdar body))
    (block scope (cdr body)))
   ((null? (cdr body))
    (closure scope (car body)))
   (else
    (apply err `("extraneous forms " ,@(cdr body) " after body")))))

(define-generator (unwrap cl) yield
  (do ((parent (closure-environment cl)
	       (parent)))
      ((not parent))
    (yield (closure parent (closure-form cl)))))

(define (function? form)
  (and (pair?      form)
       (eq?   (car form) 'function)))

(define-generator (forc cl) yield ; closure -> closure
  (let lower ((scope (closure-environment cl))
	      (form  (closure-form        cl)))
    (cond
     ((string? form)
      (lower scope `(string ,@(string->list form))))
     ((symbol? form)
      ;; Always try as-is first (lazy):
      (yield (closure scope form))
      (for entry in (scope form)
	   (for value in (forc
			  (closure (if (special? 'unquote (car entry))
				       (closure-environment (cdr entry))
				       ((env (closure-environment (cdr entry)))
					(if (special? 'quote (car entry))
					    (list 'unquote (cadar entry))
					    (car entry))
					(closure scope form)))
				   (closure-form (cdr entry))))
		(yield value))))
     ((not (pair? form))
      (yield (closure scope form)))
     ;; Function call:
     ((eq? (car form) 'quote)
      (err "incorrect quotation " form))
     ((not (pair? (cdr form)))
      (err "incorrect function call " form))
     ((not (null? (cddr form))) ; Call with multiple arguments:
      ;; Currying calls:
      (lower scope (cons (list (car form) (cadr form)) (cddr form))))
     ;; Normalized.
     (else
      ;; Always try as-is first (lazy):
      (yield (closure scope form))
      ;; Search current scope before that of argument's but prefer matching names in argument lists in with `eq?' (or `equal?'?) to match exact closure (which could only have been accessed from within argument execution) above just symbol equality.
      (for f in (forc (closure scope (car form)))
	   )
      (for argument in (forc (closure scope (cadr form)))
	   (do ((parent (closure-environment cl)
			(parent)))
	       ((not parent))
	     (closure parent (closure-form cl))
	     (for entry in (generator-append (forc (closure scope (car form)))
					     (forc (closure scope (car form))))
		  )))))))

(define (print form)
  (forc))

(define (repl scope reader)
  (for form in reader
       (let validate ((form form))
	 (cond
	  ((pair? form)
	   (validate (car form))
	   (validate (cdr form)))
	  ((vector? form)
	   (err "invalid syntax in " form))))
       (if (definition? form)
	   (apply def scope (cdr form))
	   (print port (closure scope form)))))

(define (curry f)
  (lambda (a) (lambda (b) (f a b))))

(define library
  `((define + ,(curry +))
    (define - ,(curry -))
    (define * ,(curry *))
    (define / ,(curry /))
    (define < ,(curry <))))

(define (main . files)
  (define scope (env))
  (repl scope library)
  (for file in (if (null? files)
		   '("-")
		   files)
       (if (string=? file "-")
	   (begin
	     (repl scope
		   (lambda ()
		     (newline)
		     (display "z> ")
		     (read)))
	     (newline))
	   (call-with-input-file (car file)
	     (lambda (port)
	       (repl scope (lambda () (read port))))))))

;;; zzlang.scm ends here
