#lang racket
(require csc151)

;;; File:
;;;   csv.rkt
;;; Summary:
;;;   A variety of useful procedures for dealing with CSV data.
;;; Author:
;;;   Samuel A. Rebelsky

(provide
 (contract-out
  [csv-row->list (-> string? list?)]
  [csv-row-w/sep->list (-> string? char? list?)]
  [read-csv-file (-> string? list?)]
  [read-csv-file-alt (-> string? list?)]
  [read-csv-file-w/sep (-> string? char? list?)]
  [read-csv (-> input-port? char? list?)]))

; +---------------------+--------------------------------------------
; | Exported procedures |
; +---------------------+

;;; Package:
;;;   csc151/csv
;;; Procedure:
;;;   csv-row->list
;;; Parameters:
;;;   row, a string
;;; Purpose:
;;;   Convert one row of a CSV to a list of strings.
;;; Produces:
;;;   entries, a list of strings and/or numbers
;;; Preconditions:
;;;   row represents a valid CSV row
;;; Postconditions:
;;;   (Forthcoming)
(define csv-row->list
  (lambda (row)
    (csv-row-w/sep->list row #\,)))

;;; Package:
;;;   csc151/csv
;;; Procedure:
;;;   csv-row-w/sep->list
;;; Parameters:
;;;   row, a string
;;;   sep, a character
;;; Purpose:
;;;   Convert one row of a CSV to a list of strings, assuming the elements are
;;;   separated by sep.
;;; Produces:
;;;   entries, a list of strings
;;; Preconditions:
;;;   row represents a valid CSV row with entries separated by sep
;;; Postconditions:
;;;   (Forthcoming)
;;; Ponderings:
;;;   * There's an RFC here: https://www.ietf.org/rfc/rfc4180.txt
;;;   * We're going to follow what seems to happen in Excel.
;;;   * Entries that start with a quotation mark skip that quotation mark up to the
;;;     next quotation mark.  (Any remaining quotation marks are included.)
;;; Problems:
;;;   This will almost certainly crash if there's a backslash at the end of a line.
(define csv-row-w/sep->list
  ; Convert a sequence of characters into a number or string (the
  ; two main CSV data types).  If force-string? is #t, always returns 
  ; a string.
  (let ([finish
         (lambda (chars force-string?)
           (let* ([str (list->string (reverse chars))]
                  [num (string->number str)])
             (if (and (not force-string?) num) num str)))])
    (lambda (row sep)
      (let ([char-at
             (lambda (pos)
               (if (< pos (string-length row))
                   (string-ref row pos)
                   eof))])
        (let kernel ([pos 0]
                     [entries null]
                     [entry null]
                     [quoted? #f]
                     [force-string? #f])
          (let ([ch (char-at pos)])
            (cond
              ; End of string
              [(eof-object? ch)
               (reverse (cons (finish entry force-string?) entries))]
              ; Separator: End the entry
              [(and (not quoted?) (eq? ch sep))
               (kernel (+ pos 1)
                       (cons (finish entry force-string?) entries)
                       null
                       #f
                       #f)]
              ; Quotation mark at the beginning of an entry
              [(and (null? entry) (not quoted?) (eq? ch #\"))
               (kernel (+ pos 1)
                       entries
                       null
                       #t
                       #t)]
              ; Quotation mark in the middle of an entry.  It could
              ; represent the end of the entry, or at least the
              ; quoted part of the entry.  It could be quoting
              ; the next double quote.
              [(and quoted? (eq? ch #\"))
               (let ([next (char-at (+ pos 1))])
                 (if (eq? next #\")
                     (kernel (+ pos 2)
                             entries
                             (cons #\" entry)
                             #t
                             #t)
                     (kernel (+ pos 1)
                             entries
                             entry
                             #f
                             #t)))]
              ; Everything else: Just add to the entry
              [else
               (kernel (+ pos 1)
                       entries
                       (cons ch entry)
                       quoted?
                       force-string?)])))))))

;;; Package:
;;;   csc151/csv
;;; Procedure:
;;;   read-csv-file
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Reads all of the entries from the given CSV file.
;;; Produces:
;;;   table, a list of lists
;;; Preconditions:
;;;   * fname names a valid file
;;;   * that file contains CSV
;;; Postconditions:
;;;   table provides an appropriate representation of table.
(define read-csv-file
  (lambda (fname)
    (read-csv-file-w/sep fname #\,)))

(define read-csv-file-alt
  (lambda (fname)
    (let* ([port (open-input-file fname)]
           [data (read-csv-alt port #\,)])
      (close-input-port port)
      data)))

;;; Package:
;;;   csc151/csv
;;; Procedure:
;;;   read-csv-file-w/sep
;;; Parameters:
;;;   fname, a string
;;;   sep, the character used to separate elements
;;; Purpose:
;;;   Reads all of the entries from the given CSV file.
;;; Produces:
;;;   table, a list of lists
;;; Preconditions:
;;;   * fname names a valid file
;;;   * that file contains CSV
;;; Postconditions:
;;;   table provides an appropriate representation of table.
(define read-csv-file-w/sep
  (lambda (fname sep)
    (let* ([port (open-input-file fname)]
           [result (read-csv port sep)])
      (close-input-port port)
      result)))

;;; Package:
;;;   csc151/csv
;;; Procedure:
;;;   read-csv
;;; Parameters:
;;;   port, an input port
;;;   sep, the character used to separate elements
;;; Purpose:
;;;   Reads all of the entries from the given CSV file.
;;; Produces:
;;;   table, a list of lists
;;; Preconditions:
;;;   * port contains CSV
;;; Postconditions:
;;;   table provides an appropriate representation of the contents
;;;   of the port.
(define read-csv
  (lambda (port sep)
    (let ([row (read-line port)])
      (if (eof-object? row)
          null
          (cons (csv-row-w/sep->list row sep)
                (read-csv port sep))))))

(define read-csv-alt
  (lambda (port sep)
    (if (eof-object? (peek-char port))
        null
        (cons (read-csv-row port sep)
              (read-csv-alt port sep)))))

;;; Package:
;;;   csc151/csv
;;; Procedure:
;;;   read-csv-row
;;; Parameters:
;;;   port, an input port
;;;   sep, the character used to separate entries
;;; Purpose:
;;;   Read one row from a CSV file.
;;; Produces:
;;;   row, a list of values
;;; Preconditions:
;;;   * port contains CSV
;;;   * (not (eof-object? (peek port)))
;;; Postconditions:
;;;   * row contains an appropriate representation of the next row
;;;     of data
;;;   * We have advanced the position in the port over that line
(define read-csv-row
  (lambda (port sep)
    (let kernel ([row null])
      (let ([next (peek-char port)])
        (cond
          ; End of file?  Done.
          [(eof-object? next)
           (reverse row)]
          ; DOS carriage return?  Skip it and a possible newline.  Done.
          [(char=? next #\return)
           (read-char port)
           (when (eq? (peek-char port) #\newline)
             (read-char port))
           (reverse row)]
          ; Newline?  Done.
          [(char=? next #\newline)
           (read-char port)
           (reverse row)]
          ; Normal case: Read a value and continue
          [else
           (kernel (cons (read-csv-entry port sep) row))])))))

;;; Package:
;;;   csc151/csv
;;; Procedure:
;;;   read-csv-entry
;;; Parameters:
;;;   port, an input port
;;;   sep, the character used to separate entries
;;; Purpose:
;;;   Read one entry from a CSV file, up to and including the separator.
;;; Produces:
;;;   entry, a Scheme value
;;; Preconditions:
;;;   * port contains CSV
;;;   * (not (eof-object? (peek port)))
;;; Postconditions
;;;   * entry represents the next value in the port (typically a string
;;;     or number.
;;;   * the position has advanced over that value.
(define read-csv-entry
  ; Convert a sequence of characters into a number or string (the
  ; two main CSV data types).  
  (let ([chars->datum
         (lambda (chars)
           (let* ([str (list->string (reverse chars))]
                  [num (string->number str)])
             (or num str)))])
    (lambda (port sep)
      (if (equal? (peek-char port) #\")
          (read-quoted-csv-string port)
          (let ([tmp (read-until port
                                 (list->string (list sep #\newline #\return)))])
            (skip-char port sep)
            (or (string->number tmp)
                tmp))))))

;;; Package:
;;;   csc151/csv
;;; Procedure:
;;;   read-quoted-csv-string
;;; Parameters:
;;;   port
;;; Purpose:
;;;   Reads a quoted string from a CSV file.
;;; Produces:
;;;   str, a string
;;; Preconditions:
;;;   * (char=? (peek-char port) #\")
;;;   * port contains a matching quotation mark after that quotation amrk
;;; Postconditions:
;;;   * str contains the specified string (not including the quotation marks).
;;;   * the file pointer has advanced.
(define read-quoted-csv-string
  (lambda (port)
    (when (char=? (peek-char port) #\")
      (read-char port))
    (let ([tmp (read-until port #\")])
      (skip-char port #\")
      ; Handle the special case of two double-quotation marks in a row.
      (let ([next (peek-char port)])
        (if (and (not (eof-object? next))
                 (char=? (peek-char port) #\"))
            (string-append tmp "\"" (read-quoted-csv-string port))
            tmp)))))
