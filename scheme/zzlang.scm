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
;; Global handler:
(let ((e (checkpoint)))
  (if e
      (begin
        (display "error: ")
        (report e))))


;;; Lazy lists utility

;; This utility implements "odd" streams, as defined by Wadler et al. in "How
;; to add laziness to a strict language without even being odd."

(define (lazy-list . elements)
  (if (null? elements)
      '()
      (cons (car elements)
	    (delay (lazy-list (cdr elements))))))

(define (lazy-map f stream)
  (if (null? stream)
      '()
      (cons (f (car stream))
            (delay (lazy-map f (force (cdr stream)))))))

;; TODO don't make it mapfilter
(define (lazy-filter keep? stream)
  (if (null? stream)
      '()
      (let ((keep (keep? (car stream))))
	(if keep
	    (cons (if (eqv? keep #t)
		      (car stream)
		      keep)
		  (delay (lazy-filter keep? (force (cdr stream)))))
	    (lazy-filter keep? (force (cdr stream)))))))

(define (lazy-append . streams)
  (cond
   ((null? streams)
    '())
   ((null? (cdr streams))
    (car streams))
   ((null? (car streams))
    (lazy-append (cdr streams)))
   (else
    (cons (caar streams)
	  (delay (apply lazy-append
			(force (cdar streams))
			(cdr streams)))))))

(define (lazy-concat stream)
  (cond
   ((null? stream)
    '())
   ((null? (car stream))
    (lazy-concat (force (cdr stream))))
   (else
    (cons (caar stream)
          (delay (lazy-concat
		  (cons (force (cdar stream))
                        (cdr stream))))))))

(define (lazy-bfs children roots)
  (lazy-concat
   (cons roots
	 (delay (lazy-bfs
		 children
		 (lazy-concat
		  (lazy-map children roots)))))))


(define closure? vector?)
(define (closure-form cl)
  (if (closure? cl)
      (vector-ref cl 0)
      cl))
(define (closure-environments cl)
  (if (closure? cl)
      (vector-ref cl 1)
      '()))
(define (closure form . envs)
  (vector (closure-form form)
	  (append (closure-environments form)
		  envs)))

(define (wildcard? form)
  (or (eq? '_             form)
      (and (pair?         form)
	   (eq?     ( car form) 'quote)
	   (pair    ( cdr form))
	   (null?   (cddr form))
	   (symbol? (cadr form)))))

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
    (set-car! (car env)
              (cons (cons name (closure (car body) env))
		    (if (symbol? (car body))
			;; Courtesy alias:
			(cons (cons (car body) (closure name env))
			      (caar env))
			(caar env)))))
   (else
    (err "cannot define " name))))

(define (definition? form)
  (and (pair?      form)
       (eq?   (car form) 'define)))

(define (function? f)
  (and (pair?      form)
       (eq?   (car form) 'function)))

;; Currying calls:
(define (curry form)
  (cond
   ((function? form)
    form)
   ((not (pair? form))
    form)
   ((eq? (car form) 'quote)
    (err "incorrect quotation " form))
   ((not (pair? (cdr form)))
    (err "incorrect function call " form))
   ((not (null? (cddr form)))
    ;; Call with multiple arguments:
    (curry (cons (list (car form) (cadr form))
		 (cddr form))))
   (else
    (list (curry ( car form))
	  (curry (cadr form))))))

(define (block env . body)
  (cond
   ((null? body)
    (err "no expression in block"))
   ((definition? (car body))
    (apply def env (cdar body))
    (block env (cdr body)))
   ((null? (cdr body))
    (closure (curry (car body)) env))
   (else
    (apply err `("extraneous forms " ,@(cdr body) " after body")))))

(define (local name)
  (list 'unquote name))

(define (local? form)
  (and (pair?         form)
       (eq?     ( car form) 'unquote)
       (pair    ( cdr form))
       (null?   (cddr form))
       (symbol? (cadr form))))

(define (set env . entries)
  (cons (list ; Box.
	 (do ((processed '()
			 (if (eq? '_ (car rest))
			     ;; Skip blanks:
			     processed
			     (cons
			      (cons (if (wildcard? (car rest))
					;; Localize:
					(local (cadar rest))
					(car rest))
				    (cadr entry))
			      processed)))
	      (rest entries (cddr rest)))
	     ((null? rest) processed)))
	env))

(define (descendant? ancestor candidate)
  (cond
   ((null? ancestor)
    candidate)
   ((null? candidate)
    #f)
   ((eq? (car ancestor) (car candidate))
    (descendant? (cdr ancestor)
                 (cdr candidate)))
   (else
    #f)))

(define (type form)
  (cond
   ((pair? form)
    (car form))
   ((boolean? form)
    'boolean)
   ((integer? form)
    'integer)
   ((real? form)
    'real)))

;; Search current scope before that of argument's but prefer matching names in argument lists in with `eq?' (or `equal?'?) to match exact closure (which could only have been accessed from within argument execution) above just symbol equality:
(define (dispatch f arg)
  (lazy-append
   (lazy-concat
    (lazy-map
     (lambda (arg)
       (cond
	((symbol? (cdr arg))
	 (lazy-filter
	  (lambda (f)
	    )
	  f)
	 )
	((pair? (cdr arg))
	 (lazy-filter))
	(else
	 (lazy-filter
	  (lambda (f)
	    (cond
	     ((procedure? (cdr f))
	      (cons '()
		    ((cdr f) (cdr arg))))
	     ((and (function? (cdr f))
		   (caddr f)))))
	  f))
	((integer? (cdr arg))
	 )
	((real? (cdr arg))
	 ))
       (let unwrap ((wrapped (car arg)))
	 (if (null? wrapped)
	     (delay '())
	     )))
     arg))
   ;; Wildcard patterns:
   (lazy-filter
    (lambda (f)
      (cond
       ((not (function? (cdr f)))
	#f)
       ((wildcard? (caddr f))
	(apply block
	       (set (car f)
		    (cadr (caddr f)) (car arg))
	       (cdddr f)))
       ((not (pair? (caddr f)))
	#f)
       ((wildcard? (caaddr f))
	(if (not (wildcard? (cadr (caddr f))))
	    (err "incorrect argument " (caddr f)))
	(apply block
	       (set (car f)
		    ( cadr (caaddr f))  (type (car arg))
		    (cadar (cdaddr f))        (car arg))
	       (cdddr f)))))
    f)))

;; `force' for zlang forms, named `forze' to deconflict with Scheme `force':
(define (forze form)
  (define (lookup env key)
    (define (search entries key)
      (cond
       ((null? entries)
        (wildcards (caar env) key))
       ((eq? (caar entries) key)
	(cons (car entries)
              (delay (search (cdr entries) key))))
       ((and (local? (caar entries))
             (eq? (cadaar entries) key))
        ;; Local hit:
	(cons (car entries)
              ;; Redact:
              (delay (wildcards (caar env) #f))))
       (else
        (search (cdr entries) key))))
    (define (wildcards entries key)
      (cond
       ((null? entries)
        (lookup (cdr env) key))
       ((wildcard? (caar entries))
	(cons (car entries)
              (delay (wildcards (cdr entries) key))))
       (else
        (wildcards (cdr entries) key))))
    (if (null? env)
        (lazy-filter
         (lambda (entry)
           (not (local? (car entry))))
         (apply lazy-append
                (map (lambda (extra)
		       (lookup extra key))
                     extras)))
        (search (caar env) key)))
  (cond
   ((pair? form)
    (lazy-concat
     (lazy-map
      (lambda (arg)
	(lazy-map
	 (lambda (f)
	   (cons f arg))
	 (lazy-bfs
	  forze
	  (lazy-list (closure (car call)
			      (closure-environments arg))))))
      (lazy-bfs
       forze
       (lazy-list (cadr call))))))
   ((char? form)
    (lazy-list `(character ,(char->integer form))))
   ((rational? form)
    (lazy-list (curry `(rational ,(numerator   form)
				 ,(denominator form)))))
   ((complex? form)
    (lazy-list (curry `(complex ,(real-part form)
				,(imag-part form)))))
   ((number? form)
    (err "unknown number " form))
   ((string? form)
    (lazy-list (curry `(string ,@(string->list form)))))
   ((not (closure? form))
    '())
   ((symbol? (closure-form form))
    (lazy-map
     (lambda (entry)
       (closure (closure-form (cdr entry))
		(if (local? (car entry))
		    (     car (closure-environments (cdr entry)))
		    (set (car (closure-environments (cdr entry)))
			 (car entry) form))
		(cdr (closure-environments form))))
     (apply lookup
	    (closure-form         form)
	    (closure-environments form))))
   ((pair? (closure-form form))
    (lazy-list (apply closure
		      (car (closure-form form))
		      (closure-environments form))
	       (closure (cdr (closure-form         form))
                        (car (closure-environments form)))))
   (else
    (lazy-list (closure-form form)))))

(define (print form)
  (lazy-bfs forze (lazy-list `(string ,form))))

(define (repl env input)
  (do ((form (input) (input)))
      ((eof-object? form))
    (let validate ((form form))
      (cond
       ((vector? form)
	(err "invalid syntax " form))
       ((pair? form)
	(validate (car form))
	(if (not (list? (cdr form)))
	    (err "invalid syntax " form))
	(validate (cdr form)))))
    (if (definition? form)
        (apply def env (cdr form))
        (print (closure (curry form) env)))))

(define (curried f)
  (lambda (a) (lambda (b) (f a b))))

(define lib
  `((define + ,(curried +))
    (define - ,(curried -))
    (define * ,(curried *))
    (define / ,(curried /))
    (define < ,(curried <))))

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
