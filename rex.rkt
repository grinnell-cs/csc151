#lang racket
(require csc151)
(require racket/generic)

; +---------+--------------------------------------------------------
; | Exports |
; +---------+

(provide 
 rex?

 rex->regexp
 rex->string
 
 rex-any-char
 rex-any-of
 rex-char-antiset
 rex-char-range
 rex-char-set
 rex-empty
 rex-string
 rex-start-of-string
 rex-end-of-string

 rex-concat
 rex-repeat
 rex-repeat-0
 
 rex-find-matches
 rex-matches?
 rex-replace-all
 rex-split-string
 )

; +-------+----------------------------------------------------------
; | Notes |
; +-------+

#|
Todo

[x] Write reading.
[ ] Add tests.
[ ] Figure out how to deal with replacements.
[ ] Deal with dashes
|#

; +-----------+------------------------------------------------------
; | Constants |
; +-----------+

(define PRIORITY-UNION 2)
(define PRIORITY-CONCAT 4)
(define PRIORITY-REPEAT 6)
(define PRIORITY-ATOM 8)

; +------------+-----------------------------------------------------
; | Interfaces |
; +------------+

;;; re interface
(define-generics rex
  ; (rex-prio rex) -> integer
  ; Determine the priority of rex.  Higher numbers give higher priority.
  (_rex-prio rex)
  ; (_rex-compile rex n names)
  ; Compile a regular expression.
  ; Clients should implmeent _rex-compile and call
  ; rex-compile on any sub-rexs.
  (_rex-compile rex n names)
  #:fallbacks
  [(define _rex-prio
     (lambda (rex)
       0))
   (define _rex-compile
     (lambda (rex n names)
       (error "rex-compile is not implemented for " rex)))])

;;; (rex-compile rex n names) -> (list string? integer? listof (pairof symbol? integer?))
;;;   rex : rex?
;;;   n : integer?
;;;   names : listof (pairof symbol? integer?)
;;; Compile a regular expression to standard form (the first element of the
;;; list), adding number of preceding parens and assoc of name/level
(define rex-compile
  (lambda (rex n names)
    (when (not (rex? rex))
      (error 'rex-compile "Expected a rex, received: ~e" rex))
    ; (fprintf (current-output-port) "rex-compile: Compiling ~v~n" rex)
    (_rex-compile rex n names)))

;;; (rex-prio rex) -> integer?
;;;   rex : rex?
;;; Determine the priority of rex.  See bove for details.
(define rex-prio
  (lambda (rex)
    (_rex-prio rex)))

; +--------------------+---------------------------------------------
; | Primary procedures |
; +--------------------+

;;; (rex->regexp rex) -> regexp
;;;   rex : rex?
;;; Convert a regular expression in rex form to a regular expression
;;; in regexp (or pregexp) form.
(define rex->regexp
  (lambda (rex)
    (pregexp (rex->string rex))))

;;; (rex->string rex) -> string?
;;;   rex : rex?
;;; Generate the regexp string for rex
(define rex->string
  (lambda (rex)
    (car (rex-compile rex 1 null))))

;;; (rex-find-matches rex str) -> listof string?
;;;   rex : rex?
;;;   str : string?
;;; Create a list of all non-overlapping portions of str that match rex.
(define rex-find-matches
  (lambda (rex str)
    (regexp-match* (rex->regexp rex) str)))

;;; (rex-matches? rex str) -> boolean
;;;   rex : rex?
;;;   str : string?
;;; Determine if rex matches all of str.
(define rex-matches?
  (lambda (rex str)
    (let [(re (pregexp (string-append "^(" (rex->string rex) ")$")))]
      ; (fprintf (current-output-port) "rex-matches: ~v~n" re)
      (regexp-match? re str))))

;;; (rex-replace-all rex str rep) -> string?
;;;    rex : rex?
;;;    str : string?
;;;    rep : either procedure? string?
;;; Replace all instances of of rex in str with either
;;; rep, if rep is a string, or (rep matched-text), if
;;; rep is a procedure.
;;;   If rep is a string, we reference the named portions
;;; of the pattern as {{name}}.
(define rex-replace-all
  (lambda (rex str rep)
    (let* ([tmp (rex-compile rex)]
           [pattern (car tmp)]
           [indices (caddr tmp)]
           [find-index (lambda (name)
                         (let ([match (assoc name indices)])
                           (if match
                               (cdr match)
                               (error "Could not find name" name))))]
           [replacement
            (regexp-replace* #px"\\{\\{[a-z]*\\}\\}" rep find-index)])
      replacement)))
    
;;; (rex-split-string rex str) -> boolean
;;;   rex : rex?
;;;   str : string?
;;; Split str at each instance of rex
(define rex-split-string
  (lambda (rex str)
    (string-split str (rex->regexp rex))))

; +---------+--------------------------------------------------------
; | Structs |
; +---------+

;;; (named-rex name rex) -> rex?
;;;    name : string?
;;; Create a rex that matches the same things as the given rex, but
;;; that also has a name (for replacement).
(define-struct named-rex (name rex)
  #:transparent
  #:methods gen:rex
  [(define _rex-prio (lambda (rex) PRIORITY-ATOM))
   (define _rex-compile
     (lambda (rex n names)
       (let ([tmp (rex-compile (named-rex-rex rex) (+ n 1) names)])
         (list (string-append "(" (car tmp) ")")
               (cadr tmp)
               (cons (cons (named-rex-name rex)
                           (string-append "\\" (number->string n)))
                     names)))))]
  #:guard
  (lambda (name rex type-name)
    (cond
      [(not (rex? rex))
       (error type-name "invalid rex: ~e" rex)]
      [(not (string? name))
       (error type-name "names must be strings: ~e" name)]
      [(not (regexp-match? #px"[a-z]+" name))
       (error type-name "names must contain only lowercase letters")]
      [else
       (values name rex)])))

;;; (rex-anti-range start finish) -> rex?
;;;   start : char?
;;;   finish : char?
;;; Generate a rex that matches any character *not* in the range
;;; from start to finish.
(define-struct rex-anti-range (start finish)
  #:transparent
  #:methods gen:rex
  [(define _rex-prio (lambda (rex) PRIORITY-ATOM))
   (define _rex-compile
     (lambda (rex n names)
       (list (string #\[
                     #\^
                     (rex-anti-range-start rex)
                     #\-
                     (rex-anti-range-finish rex)
                     #\])
             n
             names)))]
  #:guard
  (lambda (start finish type-name)
    (when (or (not (char? start)) (not (char? finish)))
      (error type-name
             "Expects characters, received ~e and ~e"
             start finish))
    (when (> (char->integer start) (char->integer finish))
      (error type-name
             "Invalid range: The collating sequence number for ~e (~v) is greater than the collating sequence number for ~e (~v)"
             start (char->integer start)
             finish (char->integer finish)))
    (values start finish)))

;;; (rex-any-char) : rex?
;;; Generate a regular expression that matches any character.
(define-struct rex-any-char ()
  #:transparent
  #:methods gen:rex
  [(define _rex-prio (lambda (rex) PRIORITY-ATOM))
   (define _rex-compile
     (lambda (rex n names)
       (list "." n names)))])

;;; (_rex-any-of rexs) -> rex?
;;;   rexs : listof rex?
;;; Generate the regular expression that matches any of rexs.
(define-struct _rex-any-of (rexs)
  #:transparent
  #:methods gen:rex
  [(define _rex-prio (lambda (rex) PRIORITY-UNION))
   (define _rex-compile
     (lambda (rex n names)
       (let ([prio (rex-prio rex)])
         (letrec
             ([kernel
               (lambda (rexs str n names)
                 (cond
                   [(null? rexs)
                    (list (substring str 1) n names)]
                   [(>= prio (rex-prio (car rexs)))
                    (let ([tmp (rex-compile (car rexs) (+ n 1) names)])
                      (kernel (cdr rexs)
                              (string-append str "|" "(" (car tmp) ")")
                              (cadr tmp)
                              (caddr tmp)))]
                   [else
                    (let ([tmp (rex-compile (car rexs) n names)])
                      (kernel (cdr rexs)
                              (string-append str "|" (car tmp))
                              (cadr tmp)
                              (caddr tmp)))]))])
           (kernel (_rex-any-of-rexs rex) "" n names)))))]
  #:methods gen:custom-write
  [(define write-proc
     (lambda (rex port mode)
       (let ([str-port (open-output-string)])
         (display "(rex-any-of" str-port)
         (for-each (lambda (rex)
                     (display " " str-port)
                     (print rex str-port))
                   (_rex-any-of-rexs rex))
         (display ")" str-port)
         (close-output-port str-port)
         (display (get-output-string str-port) port))))]
  #:guard
  (lambda (rexs type-name)
    (for-each (lambda (rex)
                (when (not (rex? rex))
                  (error 'rex-any-of
                         "invalid parameter ~e"
                         rex)))
              rexs)
    rexs))

;;; (rex-any-of rex1 ... rexn)
;;;   rex1: rex?
;;;   rexn: rex?
;;; Generate the regular expression that matches any of the given
;;; regular expressions.
(define rex-any-of
  (lambda rexs
    (_rex-any-of rexs)))

;;; (rex-char-antiset str) -> rex?
;;;   str : string
;;; Generate a rex that matches any character *not* in str.
(define-struct rex-char-antiset (str)
  #:transparent
  #:methods gen:rex
  [(define _rex-prio (lambda (rex) PRIORITY-ATOM))
   (define _rex-compile
     (lambda (rex n names)
       (list (string-append "[^" (string-replace (regexp-quote (rex-char-antiset-str rex)) "-" "\\-") "]")
             n
             names)))])

;;; (rex-char-range start finish) -> rex?
;;;   start : char?
;;;   finish : char?
;;; Generate a rex that matches any character in the range
;;; from start to finish.
(define-struct rex-char-range (start finish)
  #:transparent
  #:methods gen:rex
  [(define _rex-prio (lambda (rex) PRIORITY-ATOM))
   (define _rex-compile
     (lambda (rex n names)
       (list (string #\[
                     (rex-char-range-start rex)
                     #\-
                     (rex-char-range-finish rex)
                     #\])
             n
             names)))]
  #:guard
  (lambda (start finish type-name)
    (when (or (not (char? start)) (not (char? finish)))
      (error type-name
             "Expects characters, received ~e and ~e"
             start finish))
    (when (> (char->integer start) (char->integer finish))
      (error type-name
             "Invalid range: The collating sequence number for ~e (~v) is greater than the collating sequence number for ~e (~v)"
             start (char->integer start)
             finish (char->integer finish)))
    (values start finish)))

;;; (rex-char-set str) -> rex?
;;;   str : string
;;; Generate a rex that matches any single character in str.
(define-struct rex-char-set (str)
  #:transparent
  #:methods gen:rex
  [(define _rex-prio (lambda (rex) PRIORITY-ATOM))
   (define _rex-compile
     (lambda (rex n names)
       (list (string-append "[" (string-replace (regexp-quote (rex-char-set-str rex)) "-" "\\-") "]")
             n
             names)))]
  #:guard
  (lambda (str type-name)
    (when (not (string? str))
      (error type-name "expects a string, received ~e" str))
    str))

;;; (_rex-concat rexs) -> rex?
;;;   rexs : listof rex?
;;; Generate the regular expression created by concatenating rexs.
(define-struct _rex-concat (rexs)
  #:transparent
  #:methods gen:rex
  [(define _rex-prio (lambda (rex) PRIORITY-CONCAT))
   (define _rex-compile
     (lambda (rex n names)
       (let ([prio (rex-prio rex)])
         (letrec
             ([kernel
               (lambda (rexs str n names)
                 (cond
                   [(null? rexs)
                    (list str n names)]
                   [(> prio (rex-prio (car rexs)))
                    (let ([tmp (rex-compile (car rexs) (+ n 1) names)])
                      (kernel (cdr rexs)
                              (string-append str "(" (car tmp) ")")
                              (cadr tmp)
                              (caddr tmp)))]
                   [else
                    (let ([tmp (rex-compile (car rexs) n names)])
                      (kernel (cdr rexs)
                              (string-append str (car tmp))
                              (cadr tmp)
                              (caddr tmp)))]))])
           (kernel (_rex-concat-rexs rex) "" n names)))))]
  #:methods gen:custom-write
  [(define write-proc
     (lambda (rex-concat port mode)
       (let ([str-port (open-output-string)])
         (display "(rex-concat " str-port)
         (for-each (lambda (rex)
                     (display " " str-port)
                     (print rex str-port))
                   (_rex-concat-rexs rex-concat))
         (display ")" str-port)
         (close-output-port str-port)
         (display (get-output-string str-port) port))))]
  #:guard
  (lambda (rexs type-name)
    (for-each (lambda (rex)
                (when (not (rex? rex))
                  (error 'rex-concat
                         "invalid parameter ~e"
                         rex)))
              rexs)
    rexs))

;;; (rex-concat rex1 ... rexn)
;;;   rex1: rex?
;;;   rexn: rex?
;;; Generate the regular expression that represents the concatenation of
;;; rex1 ... rexn.
(define rex-concat
  (lambda rexs
    (_rex-concat rexs)))

;;; (rex-empty) : rex?
;;; Generate a regular expression that matches the empty string.
(define-struct rex-empty ()
  #:transparent
  #:methods gen:rex
  [(define _rex-prio (lambda (rex) PRIORITY-ATOM))
   (define _rex-compile
     (lambda (rex n names)
       (list "" n names)))])

;;; (rex-end-of-string) : rex?
;;; Generate a regular expression that matches the end of a string
(define-struct rex-end-of-string ()
  #:transparent
  #:methods gen:rex
  [(define _rex-prio (lambda (rex) PRIORITY-ATOM))
   (define _rex-compile
     (lambda (rex n names)
       (list "$" n names)))])

;;; (rex-repeat rex)
;;;   rex : rex?
;;; Generate a regular expression that matches 1 or more copies of rex.
(define-struct rex-repeat (rex)
  #:transparent
  #:methods gen:rex
  [(define _rex-prio (lambda (rex) PRIORITY-REPEAT))
   (define _rex-compile
     (lambda (rex n names)
       (let ([subrex (rex-repeat-rex rex)])
         ; (fprintf (current-output-port)
         ;         "rex prio: ~v; subrex prio: ~v ~n"
         ;         (rex-prio rex) (rex-prio subrex))
         (if (>= (rex-prio rex) (rex-prio subrex))
             (let ([tmp (rex-compile subrex (+ n 1) names)])
               (cons (string-append "(" (car tmp) ")+")
                     (cdr tmp)))
             (let ([tmp (rex-compile subrex n names)])
               (cons (string-append (car tmp) "+")
                     (cdr tmp)))))))])

;;; (rex-repeat rex)
;;;   rex : rex?
;;; Generate a regular expression that matches 1 or more copies of rex.
(define-struct rex-repeat-0 (rex)
  #:transparent
  #:methods gen:rex
  [(define _rex-prio (lambda (rex) PRIORITY-REPEAT))
   (define _rex-compile
     (lambda (rex n names)
       (let ([subrex (rex-repeat-0-rex rex)])
         (if (>= (rex-prio rex) (rex-prio subrex))
             (let ([tmp (rex-compile subrex (+ n 1) names)])
               (cons (string-append "(" (car tmp) ")*")
                     (cdr tmp)))
             (let ([tmp (rex-compile subrex n names)])
               (cons (string-append (car tmp) "*")
                     (cdr tmp)))))))])

;;; (rex-start-of-string) : rex?
;;; Generate a regular expression that matches the start of a string.
(define-struct rex-start-of-string ()
  #:transparent
  #:methods gen:rex
  [(define _rex-prio (lambda (rex) PRIORITY-ATOM))
   (define _rex-compile
     (lambda (rex n names)
       (list "^" n names)))])

;;; (rex-string str) -> rex?
;;;   str : string?
;;; Generate a regular expression that matches str exactly.
(define-struct rex-string (str)
  #:transparent
  #:methods gen:rex
  [(define _rex-prio (lambda (rex) PRIORITY-CONCAT))
   (define _rex-compile
     (lambda (rex n names)
       (list (regexp-quote (rex-string-str rex)) n names)))]
  #:guard
  (lambda (str type-name)
    (cond
      [(string? str)
       str]
      [else
       (error type-name
              "invalid parameter ~e"
              str)])))

