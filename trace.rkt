#lang racket

(require racket/pretty)

;;; File:
;;;   trace.rkt
;;; Author:
;;;   Samuel A. Rebelsky
;;; Summary:
;;;   A simple tracing tool for Racket.
;;; Contents:
;;;   [TBD]

(provide trace)
(provide trace-core)
(provide step)
(provide current)
(provide remaining-steps)

; +---------+--------------------------------------------------------
; | Globals |
; +---------+

;;; current-sexp : s-expression?
;;; The s-expression we're currently evaluating.
(define current-sexp "Use (trace exp) to trace an expression.")

;;; procedures : hash?
;;; All the defined procedures.

; +---------------+--------------------------------------------------
; | Local Helpers |
; +---------------+

;;; (pp sexp) -> void?
;;;    sexp : s-expression
;;; Pretty print an s expression
(define pp
  (lambda (sexp)
    (let ([tmp (pretty-format sexp 30 #:mode 'display)])
      (display (regexp-replace* "\n" tmp "\n     "))
      (newline))))
  

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
      [(symbol? sexp)
       (let ([val (eval sexp)])
         (if (procedure? val)
             (cons sexp false)
             (cons val true)))]
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
      [(equal? 'quote (car lst))
       (cons lst 'false)]
      [(equal? 'lambda (car lst))
       (cons lst 'false)]
      [(equal? 'if (car lst))
       (let ([tmp (one-step (cadr lst))])
         (if (cdr tmp)
             (cons (cons 'if (cons (car tmp) (cddr lst))) true)
             (if (car tmp)
                 (cons (caddr tmp) true)
                 (cons (cadddr tmp) true))))]
      [else
       ; (display "Evaluating\n    ") (pp lst) (newline)
       (let ([tmp (one-step-list lst)])
         (if (cdr tmp)
             tmp
             (cons (eval lst) true)))])))

;;; (one-step-list lst) -> (pair-of s-expression? boolean?)
;;;    lst : list-of s-expression?
;;; Do one evalution step for a list of values.  Return true if successful or false
;;; if there is no evaluation to do.
(define one-step-list
  (lambda (lst)
    (if (null? lst)
        (cons lst false)
        (let ([newcar (one-step (car lst))])
          (if (cdr newcar)
              (cons (cons (car newcar) (cdr lst)) true)
              (let ([newcdr (one-step-list (cdr lst))])      
                (cons (cons (car lst) (car newcdr))
                      (cdr newcdr))))))))

(define trace-core
  (lambda (sexp)
    (display "Use (step) to step through the trace.\n\n")
 
    (set! current-sexp sexp)
    (current)))

; +---------------------+--------------------------------------------
; | Exported Procedures |
; +---------------------+

(define-syntax trace
  (lambda (stx)
    (let ([info (syntax->datum stx)])
      (datum->syntax stx (list 'trace-core (list 'quote (cadr info)))))))

(define step
  (lambda ()
    (let ([result (one-step current-sexp)])
      (if (not (cdr result))
          (begin
            (display "Evaluation complete.\n")
            (display "    ")
            (pp (car result))
            (newline))
          (begin
            (set! current-sexp (car result))
            (display "--> ")
            (pp current-sexp))))))

(define current
  (lambda ()
    (display "    ")
    (pp current-sexp)))

(define remaining-steps
  (lambda ()
    (let ([result (one-step current-sexp)])
      (if (not (cdr result))
          (newline)
          (begin
            (set! current-sexp (car result))
            (display "--> ")
            (pp current-sexp)
            (remaining-steps))))))
      