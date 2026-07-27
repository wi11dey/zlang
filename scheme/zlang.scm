(define *zlang-environment* '())

(define-syntax zdefine!
  (syntax-rules ()
    ((_ name body ...)
     (set! *zlang-environment* (cons (cons 'name '(body ...)) *zlang-environment*)))))

(define (zeval form)
  (cond
   ((symbol? form)
    (assoc (cons 'quote form) *zlang-environment*))
   ((pair? form)
    )))
