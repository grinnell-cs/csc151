#lang racket

;;; File:
;;;   files.rkt
;;; Summary:
;;;   A variety of useful procedures for dealing with files.
;;; Author:
;;;   Samuel A. Rebelsky

(provide
 (contract-out
  [file->chars (-> string? (listof char?))]
  [file->lines (-> string? (listof string?))]
  [file->words (-> string? (listof string?))]
  [read-word (-> input-port? string?)]))

; +---------------------+--------------------------------------------
; | Exported procedures |
; +---------------------+

;;; Package:
;;;   csc151/files
;;; Procedure:
;;;   file->chars
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Reads and returns all of the characters in the file
;;; Produces:
;;;   chars, a list of characters
;;; Preconditions:
;;;   fname names a valid file
;;; Postconditions:
;;;   chars contains all of the characters in the file.
(define file->chars
  (lambda (fname)
    (file->stuff fname read-char eof-object?)))

;;; Package:
;;;   csc151/files
;;; Procedure:
;;;   file->lines
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Reads and returns all of the lines in the file
;;; Produces:
;;;   lines, a list of strings
;;; Preconditions:
;;;   fname names a valid file
;;; Postconditions:
;;;   lines contains all of the lines in the file, in the order they appear,
;;;   but without newlines.
(define file->line
  (lambda (fname)
    (file->stuff fname read-line eof-object?)))

;;; Package:
;;;   csc151/files
;;; Procedure:
;;;   file->words
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Reads and returns all of the words in the file
;;; Produces:
;;;   words, a list of strings
;;; Preconditions:
;;;   fname names a valid file
;;; Postconditions:
;;;   words contains all of the words in the file, in the order they appear,
;;;   but without punctuation.
(define file->words
  (lambda (fname)
    (file->stuff fname read-word (lambda (stuff) (equal? stuff "")))))

;;; Package:
;;;   csc151/files
;;; Procedure:
;;;   read-word
;;; Parameters:
;;;   port, an input port
;;; Purpose:
;;;   Reads the next word from the port
;;; Produces:
;;;   word, a string
;;; Preconditions:
;;;   port is open for reading
;;; Postconditions:
;;;   word is the next word in the port.
(define read-word
  (lambda (port)
    (let kernel ([chars null])
      (let ([ch (read-char port)])
        (cond
          [(eof-object? ch)
           (list->string (reverse chars))]
          [(word-char? ch)
           (kernel (cons ch chars))]
          [(null? chars)
           (kernel null)]
          [else
           (list->string (reverse chars))])))))

; +------------------+-----------------------------------------------
; | Local Procedures |
; +------------------+

;;; Procedure:
;;;   file->stuff
;;; Parameters:
;;;   fname, a string that names a file
;;;   read-thing, a procedure that reads from an input port
;;;   end?, a predicate
;;; Purpose:
;;;   Repeatedly reads from the file named by fname until it
;;;   encounters a value for which end? holds.
;;; Produces:
;;;   stuff, a list of things.
;;; Philosophy:
;;;   read-chars, read-lines, and read-words all had
;;;   the same structure.  This procedure attempts to
;;;   unify those structures.
(define file->stuff
  (lambda (fname read-thing end?)
    (let ([port (open-input-file fname)])
      (let kernel ()
        (let ([thing (read-thing port)])
          (cond
            [(end? thing)
             (close-input-port port)
             null]
            [else
             (cons thing (kernel))]))))))

;;; Procedure:
;;;   word-char?
;;; Parameters:
;;;   ch, a character
;;; Purpose:
;;;   Determine if ch is "word character".
;;; Produces:
;;;   ok?, a Boolean
(define word-char?
  (lambda (ch)
    (or (char-alphabetic? ch)
        (char-numeric? ch))))
