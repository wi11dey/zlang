#!/usr/local/bin/guile \
-e main -s
!#

;;; zzlang.scm --- R5RS interpreter of a subset of zlang for bootstrapping.

;; N.B. Some procedure names have been abbreviated or changed to deconflict them
;; with commonly defined procedures in Scheme implementations.


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


;;; Lazy streams utility

;; This utility implements "odd" streams, as defined by Wadler et al. in "How to add
;; laziness to a strict language without even being odd."

(define (lazy-map f stream)
  (if (null? (force stream))
      stream
      (delay (cons (f (car (force stream)))
		   (lazy-map (cdr (force stream)))))))

(define (lazy-filter keep? stream)
  (cond
   ((null? (force stream))
    stream)
   ((keep? (car (force stream)))
    (delay (cons (car (force stream))
		 (lazy-filter keep? (cdr (force stream))))))
   (else
    (lazy-filter keep? (cdr (force stream))))))

(define (lazy-append . streams)
  (cond
   ((null? streams)
    (delay '()))
   ((null? (cdr streams))
    (car streams))
   ((null? (force (car streams)))
    (lazy-append (cdr streams)))
   (else
    (delay (cons (car (force (car streams)))
		 (apply lazy-append
			(cdr (force (car streams)))
			(cdr streams)))))))

(define (lazy-concat stream)
  (cond
   ((null? (force stream))
    stream)
   ((null? (force (car (force stream))))
    (lazy-concat (cdr (force stream))))
   (else
    (delay (cons (car (force (car (force stream))))
		 (lazy-concat
		  (delay (cons (cdr (force (car (force stream))))
			       (cdr (force stream))))))))))


(define (wildcard? form)
  (and (pair?         form)
       (eq?     ( car form) 'quote)
       (pair    ( cdr form))
       (null?   (cddr form))
       (symbol? (cadr form))))

(define (def env name . body)
  (cond
   ((null? body)
    (err "no definition in (define " name ")"))
   ((pair? name)
    ;; Function definition:
    (cond
     ((pair? (car name))
      ;; Deep definition:
      (apply def env (append (car name) (cdr name))
             body))
     ((null? (cddr name))
      ;; First-class functions:
      (def env (car name)
           `(function ,(cadr name)
                      ,@body)))
     (else
      ;; Currying definitions:
      (def env (list (car name) (cadr name))
           `(define (,(car name) ,@(cddr name))
              ,@body)
           (car name)))))
   ((not (null? (cdr body)))
    (apply err `("too many definitions in (define " ,name ,@body ")")))
   ((or (symbol? name)
        (wildcard? name))
    (if (symbol? (car body))
	;; Courtesy alias:
	(set-car! (car env)
		  (cons (cons (car body) (cons env name))
			(caar env))))
    (set-car! (car env)
	      (cons (cons name (cons env (car body)))
		    (caar env))))
   (else
    (err "cannot define " name))))

(define (definition? form)
  (and (pair?      form)
       (eq?   (car form) 'define)))

(define (local name)
  (list 'unquote name))

(define (local? form)
  (and (pair?         form)
       (eq?     ( car form) 'unquote)
       (pair    ( cdr form))
       (null?   (cddr form))
       (symbol? (cadr form))))

(define (set entry form)
  (if (local? (car entry))
      (cadr entry)
      (cons (list ; Box.
	     (list
	      ;; Entry:
	      (cons (if (wildcard? entry)
			(local (cadar entry))
			(car entry))
		    (cons env form))))
	    (cadr entry))))

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

;; `force' for zlang forms, named `forze' to deconflict with Scheme `force':
;; `env' is lexical, `extras' are the only dynamic part.
(define (forze env form . extras)
  (define (lookup env key)
    (define (search entries key)
      (cond
       ((null? entries)
	(wildcards (caar env) key))
       ((eq? (caar entries) key)
	(delay (cons (car entries)
		     (search (cdr entries) key))))
       ((and (local? (caar entries))
	     (eq? (cadaar entries) key))
	;; Local hit:
	(delay (cons (car entries)
		     ;; Redact:
		     (wildcards (caar env) #f))))
       (else
	(search (cdr entries) key))))
    (define (wildcards entries key)
      (cond
       ((null? entries)
	(lookup (cdr env) key))
       ((wildcard? (caar entries))
	(delay (cons (car entries)
		     (wildcards (cdr entries) key))))
       (else
	(wildcards (cdr entries) key))))
    (if (null? env)
	(lazy-filter
	 (lambda (entry)
	   (not (local? (car entry))))
	 (apply lazy-append
		(map (lambda (env)
		       (lookup env key))
		     extras)))
	(search (caar env) key)))
  ;; Search current scope before that of argument's but prefer matching names in argument lists in with `eq?' (or `equal?'?) to match exact closure (which could only have been accessed from within argument execution) above just symbol equality:
  (define (dispatch f arg)
    (apply lazy-append
	   (let unwrap ((wrapped (car arg)))
	     (if (null? wrapped)
		 '()
		 (cons (lazy-filter
			(lambda (f)
			  )
			(apply forze
			       env
			       (car form)
			       (car arg)
			       extras))
		       (unwrap (cdr wrapped)))))))
  (let normalize ((form form))
    (cond
     ((char? form)
      (normalize `(character ,(char->integer form))))
     ((rational? form)
      (normalize `(rational ,(numerator   form)
			    ,(denominator form))))
     ((complex? form)
      (normalize `(complex ,(real-part form)
			   ,(imag-part form))))
     ((string? form)
      (normalize `(string ,@(string->list form))))
     ((symbol? form)
      (delay (cons (cons env form) ; Always try as-is first (lazy).
		   (lazy-concat
		    (lazy-map (lambda (entry)
				(apply forze
				       (set entry form)
				       (cddr entry)
				       extras))
			      (lookup env form))))))
     ((not (pair? form))
      (delay (list (cons env form))))
     ;; Function call:
     ((eq? (car form) 'quote)
      (err "incorrect quotation " form))
     ((not (pair? (cdr form)))
      (err "incorrect function call " form))
     ((not (null? (cddr form))) ; Call with multiple arguments:
      ;; Currying calls:
      (normalize (cons (list (car form) (cadr form)) (cddr form))))
     ;; Normalized.
     (else
      (delay (cons (cons env form) ; Always try as-is first (lazy).
		   (lazy-concat
		    (lazy-map
		     (lambda (arg)
		       (dispatch (cons env (car form)) arg))
		     (forze env (cadr form))))))))))

(define (print form)
  (forze))

(define (repl env input)
  (do ((form (input) (input)))
      ((eof-object? form))
    (let validate ((form form))
      (cond
       ((pair? form)
	(validate (car form))
	(validate (cdr form)))
       ((vector? form)
	(err "invalid syntax in " form))))
    (if (definition? form)
	(apply def env (cdr form))
	(print port (cons env form)))))

(define (curry f)
  (lambda (a) (lambda (b) (f a b))))

(define lib
  `((define + ,(curry +))
    (define - ,(curry -))
    (define * ,(curry *))
    (define / ,(curry /))
    (define < ,(curry <))))

(define (main . files)
  (define env (list (list '())))
  (for-each
   (lambda (defn)
     (apply def env (cdr defn)))
   lib)
  (for-each
   (lambda (file)
     (if (string=? file "-")
	 (begin
	   (repl env
		 (lambda ()
		   (newline)
		   (display "z> ")
		   (read)))
	   (newline))
	 (call-with-input-file (car file)
	   (lambda (port)
	     (repl env (lambda () (read port)))))))
   (if (null? files)
       '("-")
       files)))

;;; zzlang.scm ends here
