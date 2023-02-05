#!/usr/local/bin/guile \
-e main -s
!#

;;; zzlang.scm --- Pure R5RS interpreter of a subset of zlang for bootstrapping purposes.

;; N.B. Some procedure names have been abbreviated to deconflict them with commonly defined procedures in Scheme implementations.


;;; Exception facility

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


;;; Generator facility

;; Based off of SRFI 158 `make-coroutine-generator':
(define done (list 'done)) ; Unique object signaling generator exhaustion.
(define (done-object) done) ; Consistency with R7RS style.
(define (done-object? obj)
  (or (eq? obj done)
      (eof-object? obj) ; (read) is also a generator.
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


;;; Struct facility

(define-syntax is?
  (syntax-rules ()
    ((is? name)
     (lambda (candidate)
       (and (vector? candidate)
            (= (vector-length candidate) 2)
            (eq? (vector-ref candidate 0) 'name))))))
(define-syntax struct
  (syntax-rules ()
    ((struct (name fields ...))
     (define (name . field)
       (cond
        ((null? field)
         (lambda (fields ...)
           (vector 'name
                   (list (cons 'fields fields) ...))))
        ((null? (cdr field))
         (lambda (instance)
           (if (not ((is? name) instance))
               (err instance " is not a " 'name))
           (let ((result (assq (car field) (vector-ref instance 1))))
             (if result
                 (cdr result)
                 (err (car field) " is not a field of " 'name)))))
        (else
         (err "invalid struct access")))))))


;;; Binding

(define (wildcard? form)
  (and (pair?         form)
       (eq?     ( car form) 'quote)
       (symbol? (cadr form))
       (null?   (cddr form))))

(define (env . parent)
  (define store '())
  (define-generator (get key) yield
    (for entry in store
	 (if (eq (car entry) key)
	     (yield entry)))
    ;; Wildcards:
    (for entry in store
	 (if (wildcard? (car entry))
	     (yield (cons (cadar entry)
			  (cdr entry)))))
    ;; Up one level:
    (if (pair? parent)
	(for value in ((car parent) key)
	     (yield value))))
  (lambda (key . value)
    (cond
     ((null?      value)  (get key))
     ((null? (cdr value)) (set! store (cons (cons key (car value))
					    store)))
     (else                (err "invalid environment call")))))

(struct (closure environment form))

(define (definition? form)
  (and (pair?      form)
       (eq?   (car form) 'define)))

(define (function? form)
  (and (pair?      form)
       (eq?   (car form) 'function)))


;;; Scheme interface

(define (str s)
  (cons 'string (string->list s)))

(define (curry f)
  (lambda (a) (lambda (b) (f a b))))

(define library
  `((define + ,(curry +))
    (define - ,(curry -))
    (define * ,(curry *))
    (define / ,(curry /))
    (define < ,(curry <))))


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

;; Closures shouldn't be generated by forc (might be returned by it though), only by scope/app/block, so should never contain multiple environments. When multiple definitions, iterates over them
(define-generator (forc environments form) yield
  (let resolve ((environments environments)
                (form form))
    (if (pair? form)
        ;; Function call (assume normalized form for now, copy rules from below and make into `cond' later):
        (if (pair? (car form))
            (forc environments (car form)))
        )

    (cond
     ((closure? form)
      (if (not (function? (closure-form form)))
          ;; Unwrap closure:
          (let ((result (resolve (append (closure-environments form)
                                         environments)
                                 (closure-form form))))
            (if (not (done-object? result))
                (yield result)))))
     ((function? form)
      (yield (closure environments form)))
     ((symbol? form)
      )
     ((not (pair? form))
      form)))

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
   ;;         )
   
   ;;  collect all function definitions since last value definition of (car form)
   ;;  (if empty set
   ;;   force last value definition and recurse
   ;;   )
   
   ;;  (if last definition in env of (car form) was function
   ;;   find all the definitions of (car form) up to and not including the last non-function definition
   ;;   force the value and recurse)
   ;;  (assq (car form) env)
   ;;  (let ((forced (forc env (cadr form))))
   ;;    (if (eq? forced (cadr form))
   ;;     (if (procedure? (car form))
   ;;         (apply (car form) forced)
   ;;         ;; No match.
   ;;         )))
   ;;  )
   ((procedure? (car form))
    ;; Force until not a vector or a pair or a symbol, then pass to procedure.
    )))

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
   ((null? (cdr body))
    (apply err `("too many definitions in (define " ,name ,@body ")")))
   ((or (symbol? name)
        (wildcard? name))
    (store name (car body))
    (if (symbol? (car body))
	;; Courtesy alias:
	(store (car body) name)))
   (else
    (err "cannot define " name))))

(define (scope . body)
  (let next ((store (env))
	     (rest body))
    (cond
     ((null? body)
      (err "no expression in scope"))
     ((definition? (car body))
      (apply def store (cdar body))
      (next store (cdr body)))
     ((null? (cdr body))
      ((closure) store (car body)))
     (else
      (apply err `("extraneous forms " ,@(cdr body) " after body"))))))


;;; Entry point

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
	       (apply def store (cdr body))
	       (print form))))
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
