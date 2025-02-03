#lang racket

(require racket/pretty)
(require "lists.rkt")

;;; File:
;;;   trace.rkt
;;; Author:
;;;   Samuel A. Rebelsky
;;; Summary:
;;;   A simple tracing tool for Racket.
;;; Contents:
;;;   (trace code) - select an expression for tracing
;;;   (define/trace val exp) - add a procedure definition to the things 
;;;     that should ;;;     be traced
;;;   (trace-core sexp) - (should be hidden, but ...)
;;;   (step) - do one step of evaluation
;;;   (current) = print the current state of the traced expression
;;;   (steps) - do all the remaining steps

(provide trace)
(provide trace-core)
(provide define/trace)
(provide step)
(provide current)
(provide steps)
(provide store-proc!)

; +---------+--------------------------------------------------------
; | Globals |
; +---------+

;;; current-sexp : s-expression?
;;; The s-expression we're currently evaluating.
(define current-sexp "Use (trace exp) to trace an expression.")

;;; procedures : hash?
;;; All the defined procedures.
(define procs (make-hash))

; +---------------+--------------------------------------------------
; | Local Helpers |
; +---------------+

;;; (pp sexp) -> void?
;;;    sexp : s-expression
;;; Pretty print an s expression
(define pp
  (lambda (sexp)
    (let ([tmp (pretty-format sexp 60 #:mode 'write)])
      (display (regexp-replace* "\n" tmp "\n     "))
      (newline))))

;;; (replace-all params arglist body)
;;;    params : list-of symbol?
;;;    arglist : list-of s-expression?
;;;    body : s-expression?
;;; Replace each parameter with the corresponding argument in the body.
(define replace-all
  (lambda (params arglist body)
    (if (null? params)
        body
        (replace-all (cdr params)
                     (cdr arglist)
                     (replace-one (car params) (car arglist) body)))))

;;; (replace-one param arg sexp)
;;;    param : symbol?
;;;    arg : s-expression?
;;;    sexp : s-expression?
;;; Replace param by arg in sexp.
(define replace-one
  (lambda (param arg sexp)
    (cond
      [(pair? sexp)
       (cond
         ; No replacing values in quoted expressions
         [(equal? (car sexp) 'quote)
          sexp]
         ; No replacing parameters from lambda expressions
         [(and (equal? (car sexp) 'lambda)
               (member param (cadr sexp)))
          sexp]
         ; Replacement happens everywhere in a compound sexp
         [else
          (cons (replace-one param arg (car sexp))
                (replace-one param arg (cdr sexp)))])]
      ; Replace the symbol by the argument
      [(equal? param sexp)
       arg]
      ; A basic value, not equal to the parameter, stays as is.
      [else
       sexp])))

;;; (sexp->val sexp) -> value?
;;;   sexp : s-expression?
;;; Convert an s-expression to the corresponding value (I hope).
(define sexp->val
  (lambda (val)
    (cond
      [(equal? val 'null)
       null]
      [(and (pair? val) (equal? (car val) 'list))
       (cdr val)]
      [else
       val])))

;;; (val->sexp val) -> sexpression?
;;;   val : value?
;;; Convert a value to the corresponding s-expression
;;; (mostly intended to handle listgs).
(define val->sexp
  (lambda (val)
    (cond
      [(null? val)
       'null]
      [(list? val)
       (cons 'list val)]
      [else
       val])))

;;; (store-proc! id defn) -> void?
;;;   id : identifier?
;;;   defn : s-expression?
;;; Store one procedure
(define store-proc!
  (lambda (id defn)
    (hash-set! procs id defn)))

; +----------------------------------------+-------------------------
; | Local: Applying special function forms |
; +----------------------------------------+

;;; (apply-fun fun args) -> s-expression?
;;;   fun : s-expression?
;;;   args : list-of s-expression?
;;; Apply a function to parameters.
(define apply-fun
  (lambda (fun args)
    ; (displayln (list 'apply-fun fun args))
    (cond
      [(or (symbol? fun) (procedure? fun))
       (cons fun args)]
      [(pair? fun)
       (let ([proc (car fun)])
         (cond
           [(or (equal? proc 'compose) (equal? proc 'o))
            (apply-composition fun args)]
           [(equal? proc 'cut)
            (apply-cut fun args)]
           [(equal? proc 'lambda)
            (apply-lambda fun args)]
           [(equal? proc 'section)
            (apply-section fun args)]
           [else
            (error "Invalid procedure: " fun)]))]
      [else
       (error "Invalid procedure: " fun)])))

;;; (apply-composition fun args) -> s-expression?
;;;   fun : composition-function?
;;;   args : list-of s-expression?
;;; Apply a composition to some arguments.
(define apply-composition
  (lambda (fun args)
    (apply-composition-core (reverse (cdr fun)) (car args))))

(define apply-composition-core
  (lambda (funs args)
    ; (displayln (list 'composing funs args))
    (if (null? funs)
        args
        (apply-composition-core (cdr funs) (list (car funs) args)))))

;;; (apply-cut fun args) -> s-expression?
;;;    fun : cut-function?
;;;    args : list-of s-expression?
;;; Apply a cut function to its arguments.
;;; TODO: Implement
(define apply-cut
  (lambda (fun args)
    (cons 'list args))) ; HACK

;;; (apply-lambda fun args) -> s-expression?
;;;   fun : lambda-function?
;;;   args : list-of s-expression?
;;; Apply a lambda function to its arguments
(define apply-lambda
  (lambda (fun args)
    (let ([params (cadr fun)]
          [body (caddr fun)]
          [arglist (sexp->val args)])
      (if (= (length params) (length arglist))
          (replace-all params arglist body)
          (error "Incorrect number of parameters")))))

;;; (apply-section fun args) -> s-expression?
;;;    fun : section-function?
;;;    args : list-of s-expression?
;;; Apply a cut function to its arguments.
;;; TODO: Implement
(define apply-section
  (lambda (fun args)
    (cons 'list args))) ; HACK

; +----------------------------+-------------------------------------
; | Local: One evaluation step |
; +----------------------------+

;;; (one-step sexp) -> (pair-of s-expression? boolean?)
;;;    sexp : s-expression?
;;; Do one evaluation step.  Return true if successful or false if there is
;;; no evaluation to do.
(define one-step
  (lambda (sexp)
    (cond
      [(pair? sexp)
       (if (not (list? sexp))
           (error "Invalid expression: " sexp)
           (one-step-application sexp))]
      [(equal? sexp 'null)
       (cons sexp false)]
      [(symbol? sexp)
       (if (hash-has-key? procs sexp)
           (cons (hash-ref procs sexp) true)
           (let ([val (eval sexp)])
             (if (procedure? val)
                 (cons sexp false)
                 (cons val true))))]
      [else
       (cons sexp false)])))

;;; (one-step-application lst) -> (pair-of s-expression? boolean?)
;;;   lst : list-of s-expression?
;;; Do one step for a procedure application.
(define one-step-application
  (lambda (lst)
    (cond
      [(null? lst)
       (error "Invalid expression: empty procedure application")]
      [(member (car lst) '(quote lambda o compose cut section))
       (cons lst false)]
      [(equal? 'if (car lst))
       (let ([tmp (one-step (cadr lst))])
         (if (cdr tmp)
             (cons (cons 'if (cons (car tmp) (cddr lst))) true)
             (if (car tmp)
                 (cons (caddr lst) true)
                 (cons (cadddr lst) true))))]
      [else
       ; (display "Evaluating\n****") (pp lst) (newline)
       (let ([tmp (one-step-list lst)])
         (if (cdr tmp) 
             tmp
             (let ([fun (car lst)])
               (cond
                 [(hash-has-key? procs fun)
                  (cons (apply-fun (hash-ref procs fun) (cdr lst))
                        true)]
                 [(equal? fun 'map)
                  (one-step-map lst)]
                 [(equal? fun 'reduce)
                  (one-step-reduce lst)]
                 [(equal? fun 'reduce-left)
                  (one-step-reduce-left lst)]
                 [(equal? fun 'reduce-right)
                  (one-step-reduce-right lst)]
                 [(equal? fun 'list)
                  (if (null? (cdr lst))
                      (cons 'null true)
                      (cons lst false))]
                 [(pair? fun)
                  (cond
                    [(equal? (car fun) 'lambda)
                     (cons (apply-lambda fun (cdr lst))
                           true)]
                    [(member (car fun) '(o compose))
                     (cons (apply-composition fun (cdr lst))
                           true)]
                    [else
                     (error "Invalid function" fun)])]
                 [else
                  (let ([result (eval lst)])
                    (cons (val->sexp result) true))]))))])))

;;; (one-step-map lst) -> (cons s-expression? boolean?)
;;;   lst : listof s-sexpression? (first element is 'map)
;;; Handle the application of a map
(define one-step-map
  (let ([vals (lambda (thing) (if (equal? thing 'null) null (cdr thing)))])
    (lambda (lst)
      (let ([len (length lst)])
        (if (< len 3)
            (error "map requires a function and at least one list")
            (let ([func (cadr lst)]
                  [values (map vals (cddr lst))])
              ; (displayln (list "values" values))
              (cons (cons 'list (map (lambda (x) (cons func x)) (apply map list values)))
                    true)))))))

; +---------------------------+--------------------------------------
; | Local: One reduction step |
; +---------------------------+

;;; (one-step-reduce sexp) -> (cons s-expression? boolean?)
;;;   sexp : list-of s-expression? (of the form (reduce fun lst))
;;; Handle the application of a reduce operation
(define one-step-reduce
  (lambda (sexp)
    (one-step-reduce-general
     'reduce sexp
     (lambda (fun vals len)
       (let ([pos (random (- len 2))])
         (list 'reduce
               fun
               (cons 'list
                     (append (take vals pos)
                             (list (list fun (list-ref vals pos) (list-ref vals (+ 1 pos))))
                             (drop vals (+ pos 2))))))))))

;;; (one-step-reduce-left sexp) -> (cons s-expression? boolean?)
;;;   sexp : list-of s-expression? (of the form (reduce-left fun lst))
;;; Handle the application of a reduce-left operation
(define one-step-reduce-left
  (lambda (sexp)
    (one-step-reduce-general
     'reduce sexp
     (lambda (fun vals len)
       (list 'reduce-left fun
             (cons 'list (cons (list fun (car vals) (cadr vals))
                               (cddr vals))))))))

;;; (one-step-reduce-right sexp) -> (cons s-expression? boolean?)
;;;   sexp : list-of s-expression? (of the form (reduce-left fun lst))
;;; Handle the application of a reduce-left operation
(define one-step-reduce-right
  (lambda (sexp)
    (one-step-reduce-general
     'reduce-right sexp
     (lambda (fun vals len)
       (list 'reduce-right
             fun
             (cons 'list
                   (append (take vals (- len 2))
                           (list (list fun
                                       (list-ref vals (- len 2))
                                       (list-ref vals (- len 1)))))))))))

;;; (one-step-reduce-general name sexp redfun) -> (pair-of s-expression? boolean?)
;;;   name : symbol?
;;;   sexp : s-expression? of the form (name fun lst)
;;;   redfun : (procedure? fun list? int) -> list?
;;; One step of reduction using one of the variants of reduce (randomized, left, right, ...)
(define one-step-reduce-general
  (lambda (name sexp redfun)
    (if (not (= 3 (length sexp)))
        (error (string-append (symbol->string name) " expects two parameters: a function and a list"))
        (let* ([fun (cadr sexp)]
               [lst (caddr sexp)]
               [vals (if (equal? lst 'null) null (cdr lst))]
               [len (length vals)])
          (cond
            [(zero? len)
             (error (string-append (symbol->string name) " expects a non-empty list"))]
            [(= 1 len)
             (cons (car vals)
                   true)]
            [(= 2 len)
             (cons (cons fun vals)
                   true)]
            [else
             (cons (redfun fun vals len) true)])))))


;;; (one-step-reduce-core fun lst) -> s-expression?
;;;   fun : s-expression? (representing a binary function)
;;;   lst : list-of s-expression?
(define one-step-reduce-core
  (lambda (fun lst)
    (reduce (lambda (a b)
              (list fun a b))
            lst)))
          

;;; (one-step-list lst) -> (pair-of s-expression? boolean?)
;;;    lst : list-of s-expression?
;;; Do one evalution step for a list of values.  Return true if successful or false
;;; if there is no evaluation to do.
(define one-step-list
  (lambda (lst)
    (cond
      [(null? lst)
       (cons lst false)]
      ; [(hash-has-key? procs (car lst))
      ; (one-step-list-rest lst)]
      [else
       (let ([newcar (one-step (car lst))])
         (if (cdr newcar)
             (cons (cons (car newcar) (cdr lst)) true)
             (one-step-list-rest lst)))])))

(define one-step-list-rest
  (lambda (lst)
    (let ([newcdr (one-step-list (cdr lst))])      
      (cons (cons (car lst) (car newcdr))
            (cdr newcdr)))))

; +---------------------+--------------------------------------------
; | Exported Procedures |
; +---------------------+

;;; (trace stx) -> void?
;;;   stx : syntax?
;;; Prepare to trace the expression given by stx.
(define-syntax trace
  (lambda (stx)
    (let ([info (syntax->datum stx)])
      (datum->syntax stx (list 'trace-core (list 'quote (cadr info)))))))

(define trace-core
  (lambda (sexp)
    (display "Use (step) to step through the trace.\n\n")
 
    (set! current-sexp sexp)
    (current)))

;;; (define/trace stx) -> void?
;;;   stx : syntax?
;;; Define a procedure and keep track of it for tracing.
(define-syntax define/trace
  (lambda (stx)
    (let ([stuff (syntax->datum stx)])
      (if (= 3 (length stuff))
          (let ([identifier (cadr stuff)]
                [expression (caddr stuff)])
            ; (displayln (list 'identifier identifier 'expression expression))
            (let ([definition (list 'begin
                                    (list 'store-proc! (list 'quote identifier) (list 'quote expression))
                                    (list 'define identifier expression))])
              ; (displayln definition)
              (datum->syntax stx definition)))
          (raise-syntax-error 'define/trace "expects two parameters (identifier and expression)")))))

;;; (step) -> void?
;;; Displays the next step of evaluating the current expression.
(define step
  (lambda ()
    (let ([result (one-step current-sexp)])
      (if (cdr result)
          (begin
            (set! current-sexp (car result))
            (display "--> ")
            (pp current-sexp))
          (begin
            (display "Evaluation complete.\n")
            (display "    ")
            (pp (car result))
            (newline))))))

(define current
  (lambda ()
    (display "    ")
    (pp current-sexp)))

(define steps
  (lambda ()
    (let ([result (one-step current-sexp)])
      (if (not (cdr result))
          (newline)
          (begin
            (set! current-sexp (car result))
            (display "--> ")
            (pp current-sexp)
            (steps))))))
  
