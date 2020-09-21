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
  [lines->file (-> (listof string?) string? void?)]
  [string->file (-> string? string? void?)]
  [read-word (-> input-port? string?)]
  [skip-char (-> input-port? char? boolean?)]
  [read-until (-> input-port? (or/c procedure? char? string?) string?)]))

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
(define file->lines
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
;;;   lines->file
;;; Parameter:
;;;   lines, a list of strings
;;;   fname, a string
;;; Purpose:
;;;   Saves the given lines to the named file.
;;; Preconditions:
;;;   The fname must name a writeable file.
;;; Postconditions:
;;;   The given file now contains all the entries in lines.
(define lines->file
  (lambda (lines fname)
    (let ([port (open-output-file fname #:exists 'replace)])
      (for-each (lambda (line) (display line port) (newline port))
                lines)
      (close-output-port port))))

;;; Package:
;;;   csc151/files
;;; Procedure:
;;;   string->file
;;; Parameters:
;;;   str, a string
;;;   fname, a string
;;; Purpose:
;;;   Save the given string to the named file.
;;; Produces:
;;;   [Nothing; called for the side effect.]
;;; Preconditions:
;;;   The fname must name a writeable file.
;;; Postconditions:
;;;   The named file now contains str.
;;; Precautions:
;;;   Can clobber files.  Be careful.
;;; Problems:
;;;   May add an extra newline.
(define string->file
  (lambda (str fname)
    (let ([port (open-output-file fname #:exists 'replace)])
      (display str port)
      (close-output-port port))))

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

;;; Package:
;;;   csc151/files
;;; Procedure:
;;;   read-until
;;; Parameters:
;;;   port, an input port
;;;   terminator, a character, string, or predicate
;;; Purpose:
;;;   Read the characters until you reach terminator (or eof) (or
;;;   the predicate holds..
;;; Produces:
;;;   str, a string
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   * str represents the sequence of characters from the file pointer
;;;     of port at the beginning until (a) the first occurence of terminator,
;;;     if terminator is a character, (b) a character that appears in terminator,
;;;     it terminator is a string, or (c) a character for which terminator
;;;     holds, if terminator is a predicate.  str does not include that
;;;     character.
;;;   * the file pointer has been advanced appropriately.
(define read-until
  (lambda (port terminator)
    (let [(pred? (cond
                   [(char? terminator)
                     (lambda (ch) (char=? ch terminator))]
                   [(string? terminator)
                    (lambda (ch) (string-contains? terminator (string ch)))]
                   [(procedure? terminator)
                    terminator]
                   [else
                    (error "Invalid parameter"
                           terminator
                           "expected a character, string, or unary predicate")]))]
      (let kernel ([chars null])
        (let ([ch (peek-char port)])
          (if (or (eof-object? ch) (pred? ch))
              (list->string (reverse chars))
              (kernel (cons (read-char port) chars))))))))

;;; Package:
;;;   csc151/files
;;; Procedure:
;;;   skip-char
;;; Parameters:
;;;   port, an open input port
;;;   ch, a character
;;; Purpose:
;;;   Skip over the next character in the input file if it is ch.
;;; Produces:
;;;   skipped?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   * If (peek-char port) is ch, reads over the character and returns #t
;;;   * Otherwise, leaves the port unchanged and returns #f
(define skip-char
  (lambda (port ch)
    (let ([next (peek-char port)])
      (cond
        [(and (not (eof-object? next))
              (char=? next ch))
         (read-char port)
         #t]
        [else
         #f]))))

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

