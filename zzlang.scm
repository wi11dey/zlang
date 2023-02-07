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

(define (env . parent)
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
    ;; Up one level:
    (if (pair? parent)
	(for value in ((car parent) exported)
	     (yield value))))
  (define (self key . value)
    (cond
     ((null? value)
      (get key))
     ((and (pair? value)
	   (null? (cdr value)))
      (set! store (cons (cons key (car value))
			store))
      ;; Allow for chaining:
      self)
     (else
      (err "invalid environment call"))))
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

(define (def store name . body)
  (cond
   ((null? body)
    (err "no definition in (define " name ")"))
   ((pair? name)
    ;; Function definition:
    (cond
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
   ((not (null? (cdr body)))
    (apply err `("too many definitions in (define " ,name ,@body ")")))
   ((or (symbol? name)
	;; Wildcard:
        (special? 'quote name))
    (store name (closure store (car body)))
    (if (symbol? (car body))
	;; Courtesy alias:
	(def store (car body) name)))
   (else
    (err "cannot define " name))))

(define (definition? form)
  (and (pair?      form)
       (eq?   (car form) 'define)))

(define (scope store . body)
  (cond
   ((null? body)
    (err "no expression in scope"))
   ((definition? (car body))
    (apply def store (cdar body))
    (scope store (cdr body)))
   ((null? (cdr body))
    (closure store (car body)))
   (else
    (apply err `("extraneous forms " ,@(cdr body) " after body")))))

(define (function? form)
  (or (procedure?      form)
      (and (pair?      form)
	   (eq?   (car form) 'function))))

(define-generator (forc cl) yield
  (let lower ((store (closure-environment cl))
	      (form  (closure-form        cl)))
    (cond
     ((string? form)
      (lower store `(string ,@(string->list form))))
     ((symbol? form)
      ;; Always try as-is first (lazy):
      (yield (closure store form))
      (for entry in (store form)
	   (for value in (forc
			  (closure (if (special? 'unquote (car entry))
				       (closure-environment (cdr entry))
				       ((env (closure-environment (cdr entry)))
					(if (special? 'quote (car entry))
					    (list 'unquote (cadar entry))
					    (car entry))
					(closure store form)))
				   (closure-form (cdr entry))))
		(yield value))))
     ((not (pair? form))
      (yield (closure store form)))
     ;; Function call:
     ((eq? (car form) 'quote)
      (err "incorrect quotation " form))
     ((not (pair? (cdr form)))
      (err "incorrect function call " form))
     ((not (null? (cddr form))) ; Call with multiple arguments:
      ;; Currying calls:
      (lower store (cons (list (car form) (cadr form)) (cddr form))))
     ;; Normalized.
     (else
      ;; Always try as-is first (lazy):
      (yield (closure store form))
      ;; Argument:
      (if (closure? (cadr form))
	  )
      ;; Function:
      (for f in (forc (closure store (car form)))
	   )
      ))))


;;; Entry point

(define (curry f)
  (lambda (a) (lambda (b) (f a b))))

(define library
  `((define + ,(curry +))
    (define - ,(curry -))
    (define * ,(curry *))
    (define / ,(curry /))
    (define < ,(curry <))))

(define (main . files)
  (define (print form)
    (forc))
  (let ((store (env)))
    (define (repl input)
      (for form in input
	   (let validate ((form form))
	     (cond
	      ((pair? form)
	       (validate (car form))
	       (validate (cdr form)))
	      ((vector? form)
	       (err "invalid syntax in " form))))
	   (if (definition? form)
	       (apply def store (cdr form))
	       (print (closure store form)))))
    (repl library)
    (map (lambda (file)
	   (if (string=? file "-")
	       (begin
		 (repl (lambda ()
			 (newline)
			 (display "z> ")
			 (read)))
		 (newline))
	       (call-with-input-file (car file)
		 (lambda (port)
		   (repl (lambda () (read port)))))))
	 (if (null? files) '("-") files))))

;;; zzlang.scm ends here
