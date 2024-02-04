#lang racket

;;; File:
;;;   sstruct.rkt
;;; Summary:
;;;   Sam's extensions to Racket structs.
;;; Author:
;;;   Samuel A. Rebelsky

(provide (all-defined-out))

;;; (set-sstruct-tag! tag arity proc) -> void?
;;;   tag : keyword
;;;   arity : non-negative-integer?
;;;   proc : (name param_1 ... param_arity) -> list?
;;; Set the response to a particular tag.

;;; (sstruct name ...) -> void?
;;; Generate a structure definition by processing all the tags
;;; in the parameter list.


;;; (sstruct-tags) -> hash?
;;; Get all of the sstruct tags.
;;;
;;; Probably not the best idea, but useful.

(define-syntaxes (sstruct set-sstruct-tag! sstruct-tags)
  (letrec ([tags (make-hash)]
           [set-tag! (lambda (tag arity code)
                       (hash-set! tag (list arity code)))]
           [take (lambda (lst n)
                   (if (zero? n)
                       null
                       (cons (car lst) (take (cdr lst) (- n 1)))))]
           [drop (lambda (lst n)
                   (if (zero? n)
                       lst
                       (drop (cdr lst) (- n 1))))]
           [sstruct-process
            (lambda (name info)
              (cond
                [(null? info)
                 null]
                [(hash-ref tags (car info) #f)
                 (let* ([arity-plus-proc (hash-ref tags (car info))]
                        [arity (car arity-plus-proc)]
                        [proc (cadr arity-plus-proc)])
                   (append (apply (eval proc) (cons name (take (cdr info) arity)))
                           (sstruct-process name (drop (cdr info) arity))))]
                [else
                 (cons (car info) (sstruct-process name (cdr info)))]))])
    (values
     ; sstruct
     (lambda (stx)
       (let* ([data (syntax->datum stx)]
              [name (cadr data)]
              [info (cddr data)])
         ; (displayln data)
         (let ([result (cons 'struct (cons name (sstruct-process name info)))])
           ; (displayln result)
           (datum->syntax stx result))))
     ; sstruct-tag!
     (lambda (stx)
       (let* ([data (syntax->datum stx)]
              [tag (cadr data)]
              [info (cddr data)])
         ; (displayln (format "Setting tag ~a" tag))
         (hash-set! tags tag info))
       (datum->syntax stx '(void)))
     ; sstruct-tags
     (lambda (stx)
       (datum->syntax stx tags)))))

; +----------------+-------------------------------------------------
; | The #:done tag |
; +----------------+

;;; (permit-done) -> (void)
;;; Permit the "#:done" tag.
(define-syntax-rule (permit-done)
  (set-sstruct-tag! #:done 0
                  (lambda (name)
                    null)))
