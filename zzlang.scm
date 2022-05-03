#!/usr/local/bin/guile \
-e main -s
!#

;;; zzlang.scm --- R5RS-compliant interpreter for a subset of zlang for bootstrapping purposes.
;; Do NOT use for any other purpose. zzlang is unoptimized, outputs only binary, may silently ignore some invalid inputs, does not guarantee termination, and has limited error-handling capabilities.
;; Additionally, zzlang uses some Scheme tricks to simplify implementation at the cost of performance, which is not a concern for bootstrapping.

;; TODO learn higher category theory and H-M type system with overloading, and make zlang <-> text system before continuing implementation

;;; Utilities:

(define-syntax define-tail-cons
  ;; Transforms tail-recursive-modulo-cons functions into properly tail-recursive ones.
  (syntax-rules (lambda)
    ((define-tail-cons (name . args) . body)
     (define (name . args)
       (letrec ((f (lambda (head last . args)
		     (let* ((recurse #f)
			    (name (lambda new-args
				    (set! recurse new-args)
				    '()))
			    (return (begin . body)))
		       (set-cdr! last return)
		       (if recurse
			   (apply f head return recurse)
			   head))))
		(head (cons '() '())))
	 (cdr (f head head . args)))))
    ((define-tail-cons  name (lambda args . body))
     (define-tail-cons (name . args) . body))))

(define (string-replace s char1 char2)
  (set! s (string-copy s))
  (do ((i 0 (+ i 1)))
      ((= i (string-length s)) s)
    (if (char=? (string-ref s i) char1)
	(string-set! s i char2))))

(define (string-split separator s)
  (define-tail-cons (splitter start end length)
    (if (< end length)
	(if (char=? (string-ref s end) separator)
	    (cons (substring s start end)
		  (splitter s (+ end 1) (+ end 1) length))
	    (splitter s start (+ end 1) length))
	(list (substring (if (< start length)
			     start length)
			 length))))
  (splitter 0 0 (string-length s)))

(define (string-join separator . strings)
  (if (null? strings)
      ""
      (apply string-append
	     (car strings)
	     (let ((string-separator (string separator)))
	       (map (lambda (s)
		      (string-append separator s))
		    (cdr strings))))))

(define-tail-cons (read-forms file)
  (let ((form (read file)))
    (if (eof-object? form)
	'()
	(cons form (read-forms file)))))

(define-syntax push
  (syntax-rules ()
    ((push newelt place)
     (set! place (cons newelt place)))))

;; Uses `eval' to use syntax-rules mechanism to pattern-match data known only at runtime.
(define-syntax define-match
  (syntax-rules (quote)
    ((define-match name
       '(literal ...)
       (pattern . then) ...)
     (define (name x)
       (eval
	`(letrec-syntax
	     ((name ; Slight optimization to remove recursive `eval'.
	       (syntax-rules (quote)
		 ((name 'y)
		  (matcher . y))))
	      (matcher
	       (syntax-rules (literal ...)
		 ((matcher . pattern)
		  (begin . then)) ...)))
	   (matcher . ,x))
	(interaction-environment))))
    ((define-match name . rules)
     (define-match name '() . rules))))


;;; Implementation:

(define error #f)
(let ((error-message
       (call-with-current-continuation
	(lambda (cont)
	  (set! error (lambda args
			(cont args)))
	  #f))))
  (if error-message
      (display "error: ")
      (for-each (lambda (part)
		  (display part))
		error-message)
      (newline)))

(define (zz-print list)
  (for-each
   (lambda (element)
     (let ((hex (number->string element 16)))
       (if (odd? (string-length hex))
           (set! hex (string-append "0" hex)))
       (do ((i 0 (+ i 2)))
           ((= i (string-length hex)))
         (write-char
	  (integer->char
	   (string->number
	    (substring hex i (+ i 2))
	    16))))))
   list))

;; (define (zz-intern identifier)
;;   (string->symbol (string-append "-zz-" (symbol->string identifier))))

;; (define (zz-define identifier . definition)
;;   (eval `(define ,(zz-intern identifier)
;; 	   ,@definition)
;; 	(interaction-environment)))

;; (define (zz-lookup identifier)
;;   (eval `,(zz-intern identifier)
;; 	(interaction-environment)))

(define zz-definitions
  '((var1 . #(definition))
    (var2 . #(5))
    (func1 . ((`((argtype ,arg) #f) . body)
	      (`(#t (argtype ,arg)) . body)))))
(define zz-wildcard
  '((`((argtype ,arg) #f) . body)
    (`(#t (argtype ,arg)) . body)))

(define (zz-insert new definitions)
  (define-match zz-<
    '(function quote)
    (((function args1 _) . (function args2 . _))
     )
    (((function . _) . nonfunction)
     #f)
    ((nonfunction       . (function . _))
     #t)
    (('()))
    ((a               . b)
     (error "cannot redefine")))
  (if (null? definitions)
      (list new)
      (do ((head definitions (cdr head)))
	  ((zz-< head))))
  )

(define-match zz-eval
  '(define quote quasiquote unquote function apply)
  ((define '(f . args) . body) (zz-eval '(define 'f (function 'args . body))))
  ((define `(f . args) . body) (zz-eval '(define `f (function `args . body))))
  ((define `,var value)
   (push (zz-eval 'value) zz-wildcard)
   ;; Wildcard definition.
   ;; Only one possible value in any given scope for the wildcard variable, but can be many for functions depending on argument categories.
   )
  ((define `var value) (zz-eval '(define 'var value)))
  ((define 'var value)
   (if (not (symbol? var))
       (error "cannot define non-symbol " var))
   (push (cons var (zz-eval 'value)) zz-definitions)
   ;; If the value is a function:
   
   ;; If value is not a function, then the following works:
   (zz-define var (zz-eval 'value)))
  ((function args . body)
   '(function args . body))
  ('quoted
   'quoted)
  ((apply f . args)
   (let ((definition (assq 'f zz-definitions)))
     (if definition

	 (error 'f " undeclared"))))
  ((f . args)
   (zz-eval '(apply f . args)))
  (constant
   'constant))

(define (zz-repl port print)
  (do ((form   ''()       (read))
       (result  '()       (zz-eval form)))
      ((eof-object? form) (print result))))

(define (main . files)
  ;; Bootstrap file:
  (call-with-input-file (car files)
    (lambda (file) (zz-repl file (lambda _ _))))
  ;; Library files:
  (let ((filename->module
	 (lambda (filename)
	   (map string->symbol
		(string-split #\/ (substring
				   filename
				   0 (- (string-length filename) 3)) ; Remove .zl extension.
			      )))))
    (for-each
     (lambda (filename)
       (zz-eval `(file
		  (module ,@(file->module))
		  ,@(call-with-input-file filename read-forms))))
     (cdr files)))
  ;; Executable file:
  (zz-repl (current-input-port) zz-print))

;;; zzlang.scm ends here
