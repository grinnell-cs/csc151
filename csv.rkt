#lang racket

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
  (let ([finish
         (lambda (chars force-string?)
           (let* ([str (list->string (reverse chars))]
                  [num (string->number str)])
             (if (and (not force-string?) num) num str)))])
    (lambda (row sep)
      (let kernel ([pos 0]
                   [entries null]
                   [entry null]
                   [quoted? #f]
                   [force-string? #f])
        (if (>= pos (string-length row))
            (reverse (cons (finish entry force-string?) entries))
            (let ([ch (string-ref row pos)])
              (cond
                ; Separator: End the entry
                [(and (not quoted?) (eq? ch sep))
                 (kernel (+ pos 1)
                         (cons (finish entry force-string?) entries)
                         null
                         #f
                         #f)]
                ; Quotation mark at the beginning of an entry
                [(and (null? entry) (eq? ch #\"))
                 (kernel (+ pos 1)
                         entries
                         null
                         #t
                         #t)]
                ; Quotation mark in the middle of an entry.  It matches and we're done
                ; with the quote.  I think.
                [(and quoted? (eq? ch #\"))
                 (kernel (+ pos 1)
                         entries
                         entry
                         #f
                         #t)]
                ; Backslash in the middle of an entry,  It means to treat the next thing
                ; verbatin.  I think that holds in every case.
                [(eq? ch #\\)
                 (kernel (+ pos 2)
                         entries
                         (cons (string-ref row (+ pos 1)) entry)
                         quoted?
                         force-string?)]
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
;;;   port
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

