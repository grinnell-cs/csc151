#lang racket

;;; File:
;;;   web-utils.rkt
;;; Summary:
;;;   A variety of useful procedures for processing Web pages in Racket.
;;; Author:
;;;   Samuel A. Rebelsky

(require html-parsing)
(require html-writing)
(require net/url)
(require sxml)
(require "hop.rkt")    ; for all and o, among other things
(require "lists.rkt")  ; for reduce, among other things

(provide (all-defined-out))

; +--------+---------------------------------------------------------
; | Files! |
; +--------+

;;; Procedure:
;;;   file->xml
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Convert a file to an xexp representation of an HTML page.
;;; Produces:
;;;   html, an xexp expression
;;; Preconditions:
;;;   The given file exists and is readable.
;;;   The given file contains HTML
;;; Postconditions:
;;;   html represents the page.
(define file->xml
  (lambda (fname)
    (let* ([port (open-input-file fname)]
           [result (merge-multiple-classes
                    (xexp/top-element
                     (html->xexp port)))])
      (close-input-port port)
      result)))

;;; Procedure:
;;;   xml->file
;;; Parameters:
;;;   html, an xexp expression
;;;   fname, a string
;;; Purpose:
;;;   Saves html to the given file.
;;; Produces:
;;;   [Nothing; called for the side effect.]
(define xml->file
  (lambda (html fname)
    (let ([port (open-output-file fname)])
      (write-html html port)
      (close-output-port port))))

;;; Procedure:
;;;   string->xml
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   Convert a string representation of HTML to the internal
;;;   representation.
;;; Produces:
;;;   html, an xexp expression
;;; Preconditions:
;;;   html contains relatively valid HTML.
;;; Postconditions:
;;;  (string->xml (xml->string html)) is approximately html
(define string->xml
  (lambda (str)
    (merge-multiple-classes (xexp/top-element (html->xexp str)))))

;;; Procedure:
;;;   xml->string
;;; Parameters:
;;;   html, an xexp expression
;;; Purpose:
;;;   Converts the expression back to a string for easy reading.
;;; Produces:
;;;   str, a string
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;  (string->xml (xml->string html)) is approximately html
(define xml->string
  (lambda (html)
    (let ([port (open-output-string)])
      (write-html html port)
      (get-output-string port))))

; +-------------------+----------------------------------------------
; | xexp manipulation |
; +-------------------+

;;; Procedure:
;;;   xexp/attributes-fix
;;; Parameters:
;;;   attributes, a portion of an xexp expression representing
;;;   attributes
;;; Purpose:
;;;   Deal with a small problem in how html-parsing handles empty attributes
;;; Produces:
;;;   fixed-attributes, an equivalent set of attributes
;;; Preconditions:
;;;   attributes is of the form `((name value) ... (name) ....)
;;; Postconditions:
;;;   Any element of the form `(name) is replaced by `(name "")
(define xexp/attributes-fix 
  (lambda (attributes)
    (cond
      [(null? attributes)
       null]
      [(and (pair? (car attributes))
            (null? (cdar attributes)))
       (cons (list (caar attributes) "")
             (xexp/attributes-fix (cdr attributes)))]
      [else
       (cons (car attributes)
             (xexp/attributes-fix (cdr attributes)))])))

;;; Procedure:
;;;   xexp-cleanup
;;; Parameters:
;;;   xexp, an xexp expression
;;; Purpose:
;;;   Deal with a few potential issues with xexps, either those generated
;;;   by xml->xexp or those written by students.
;;; Produces:
;;;   clean, an xexp expression
(define xexp/cleanup
  (lambda (xexp)
    (cond
      ; Two little hacks to help my students
      [(number? xexp)
       (number->string xexp)]
      [(symbol? xexp)
       (symbol->string xexp)]
      ; Non-pairs should stay as is
      [(not (pair? xexp))
       xexp]
      ; Attributes
      [(eq? (car xexp) '@)
       (cons '@ (xexp/attributes-fix (cdr xexp)))]
      ; Entities
      [(eq? (car xexp) '&)
       xexp]
      ; Everything else
      [else
       (cons (car xexp)
             (map xexp/cleanup (cdr xexp)))])))

;;; Procedure:
;;;   xexp/top-element
;;; Parameters:
;;;   top, a top-level xexp.
;;; Purpose:
;;;   Extract the element from a top-level xexp.
;;; Produces:
;;;   element, the primary element
;;; Preconditions:
;;;   top has the form '(*TOP* ... element ...), where the values other
;;;   than the element must either be (a) strings, (b) processing instructions
;;;   of the form '(*PI* ...), (c) comments of the form '(*COMMENT* ...),
;;;   (d) annotations of the form '(@ ...).
;;; Postconditions
;;;   The first element in top what is not of the given form.
;;; Ponderings:
;;;   The official syntax of SXML/xexp is at
;;;     http://okmij.org/ftp/Scheme/SXML.html#Grammar.
;;;   That syntax has the nice property that the last item in the list
;;;   is the element.  However, html-parsing seems to allow things
;;;   after the element.
;;;     > (xml->xexp "<input type='text'/>\n\n\n")
;;;     '(*TOP* (input (@ (type "text"))) "\n" "\n" "\n")
(define xexp/top-element
  (lambda (top)
    (if (or (not (list? top))
            (null? top)
            (not (symbol? (car top)))
            (not (string-ci=? "*TOP*" (symbol->string (car top)))))
        (error "xexp/top-element: expected a top-level element, received" top)
        (let kernel ([rest (cdr top)])
          (cond
            [(null? rest)
             (error "xexp/top-element: Could not find an element")]
            [(not (pair? (car rest)))
             (kernel (cdr rest))]
            [(not (symbol? (caar rest)))
             (kernel (cdr rest))]
            [(string-contains? "*@"
                               (substring (symbol->string (caar rest)) 0 1))
             (kernel (cdr rest))]
            [else
             (car rest)])))))

;;; Procedure:
;;;   xexp->xexpr
;;; Parameters:
;;;   xexp, an xexp
;;; Purpose:
;;;   Convert xexp to an xexpr
;;; Produces:
;;;   xexpr, a corresponding xexpr
(define xexp->xexpr
  (lambda (xexp)
    (let kernel ([xexp (xexp/cleanup xexp)])
      (cond
        ; Two little hacks to help my students
        [(number? xexp)
         (number->string xexp)]
        [(symbol? xexp)
         (symbol->string xexp)]
        ; Non-pairs should stay as is
        [(not (pair? xexp))
         xexp]
        ; If we start with *TOP*, the stuff we care about is at the end.
        [(eq? (car xexp) '*TOP*)
         (kernel (xexp/top-element xexp))]
        ; Drop the @ for parameters
        [(eq? (car xexp) '@)
         (cdr xexp)]
        [else
         (cons (car xexp)
               (map kernel (cdr xexp)))]))))

; +-------------+-------------------------------------------------
; | Fetch pages |
; +-------------+

;;; Procedure:
;;;   fetch-page-pure
;;; Parameters:
;;;   url, a string that represents a URL
;;; Purpose:
;;;   Fetch the page at the given URL.
;;; Produces:
;;;   page, an xexp expression
(define fetch-page-pure
  (lambda (url)
    (if (regexp-match #rx"^http" url)
        (xexp/top-element (fetch-page-top url))
        `(html
          (head (title "Invalid URL"))
          (body
           (h1 "Invalid URL")
           (p "Could not process the URL '" ,url "'"))))))

(define fetch-page-top
  (lambda (url)
    (html->xexp (get-pure-port (string->url url)))))

;;; Procedure:
;;;   fetch-page
;;; Parameters:
;;;   url, a string representing a URL
;;; Purpose:
;;;   Fetch the page at the given URL, rewriting internal URLs so that
;;;   it appears correctly.
;;; Produces:
;;;   page, an xexp expression
;;; Problems:
;;;   Not particularly robust.
(define fetch-page
  (lambda (url)
    (update-urls (merge-multiple-classes (fetch-page-pure url))
                 url)))

;;; Procedure:
;;;   fetch-page-as-HTML
;;; Parameters:
;;;   url, a string representing a URL
;;; Purpose:
;;;   Fetch the page at the given URL.
;;; Produces:
;;;   page, a string
;;; Problems:
;;;   Not particularly robust.
(define fetch-page-as-html
  (lambda (url)
    (let ([port (get-pure-port (string->url url))])
      (let kernel ()
        (let ([line (read-line port)])
          (cond
            [(eof-object? line)
             (close-input-port port)
             null]
            [else
             (cons line (kernel))]))))))

; +--------------------------------+---------------------------------
; | Extract information from pages |
; +--------------------------------+

;;; Procedure:
;;;   extract-text
;;; Parameters:
;;;   xexp, an xexp
;;; Purpose:
;;;   Extract the text (string) from an xexp
;;; Produces:
;;;   text, a string
(define extract-text
  (lambda (xexp)
    ((sxpath "string(/)")
     (page-delete-comments
      (page-delete-elements xexp 'script)))))

;;; Procedure:
;;;   extract-by-tag
;;; Parameters:
;;;   tag, a symbol
;;;   xexp, an xexp
;;; Purpose:
;;;   Extract all of the expression in exp that start with the given
;;;   tag.
;;; Produces:
;;;   expressions, a list of xexps
(define extract-by-tag
  (lambda (tag xexp)
    ((sxpath (string-append "//" (symbol->string tag))) xexp)))

;;; Procedure:
;;;   extract-attribute
;;; Parameters:
;;;   attribute, a symbol
;;;   xexp, an xexp
;;; Purpose:
;;;   Extract the given attribute from the expression.
(define extract-attribute
  (lambda (attribute xexp)
    (let ([default ""])
      (cond
        [(not (pair? xexp))
         default]
        [(null? (cdr xexp))
         default]
        [(not (pair? (cadr xexp)))
         default]
        [(not (eq? '@ (caadr xexp)))
         default]
        [else
         (let kernel ([attributes (cdadr xexp)])
           (cond 
             [(null? attributes)
              default]
             [(eq? attribute (caar attributes))
              (cadar attributes)]
             [else
              (kernel (cdr attributes))]))]))))

;;; Procedure:
;;;   sxpath-match
;;; Parameters:
;;;   path, a string
;;;   xexp, an xexp expression
;;; Purpose:
;;;   Identify all of the elements of xexp that match
;;;   the given path.
;;; Produces:
;;;   matches, a list of elements
(define sxpath-match
  (lambda (path xexp)
    ((sxpath path) (add-top xexp))))

; +-----------------+------------------------------------------------
; | Transform pages |
; +-----------------+

;;; Procedure:
;;;   page-transform-elements
;;; Parameters:
;;;   page, a page in the standard xexp format
;;;   tag, a symbol
;;;   transform, a procedure from elements to elements
;;; Purpose:
;;;   Transform all of the elements that start with the given tag
;;; Produces:
;;;   transformed, a page in the standard xexp format
(define page-transform-elements
  (lambda (page tag transform)
    ((sxml:modify (list (string-append "//"
                                       (symbol->string tag))
                        ; sxml:modify expects a three parameter proc;
                        ; we expect a one-parameter proc.
                        (lambda (element context root)
                          (transform element))))
     page)))

;;; Procedure:
;;;   page-delete-elements
;;; Parameters:
;;;   page, a page in the standard xexp format
;;;   tag, a symbol
;;; Purpose:
;;;   Delete all of the elements with the given tag
;;; Produces:
;;;   newpage, a page in the standard xexp format
(define page-delete-elements
  (lambda (page tag)
    ((sxml:modify (list (string-append "//"
                                       (symbol->string tag))
                        'delete))
     page)))

;;; Procedure:
;;;   page-delete-comments
;;; Parameters:
;;;   page, a page in the standard xexp format
;;;   tag, a symbol
;;; Purpose:
;;;   Delete all the comments
;;; Produces:
;;;   newapage, a page in the standard xexp format
;;; Ponderings:
;;;   Comments seem to take the form `(*COMMENT* "The comment").  But I
;;;   can't use *COMMENT* in an SXpath.  This serves as an alternative.
(define page-delete-comments
  (sxml:modify (list "//comment()" 'delete)))

;;; Procedure:
;;;   page-replace-text
;;; Parameters:
;;;   page, a page in the standard xexp format
;;;   pattern, a string or a regular expression
;;;   replacement, a string (for now)
;;; Purpose:
;;;   Replace all instances of pattern in the text by replacement
;;; Produces:
;;;   newpage, a page in the standard xexp format
;;; Problem:
;;;   Does not yet handle the situation in which replacement is a
;;;   replacement element.
(define page-replace-text
  (lambda (page pattern replacement)
    (if (pair? replacement)
        (page-replace-text page pattern
                           (lambda (element) replacement))
        (let* ([rxpattern (if (regexp? pattern)
                              pattern
                              (regexp pattern))]
               [proc (cond
                       [(string? replacement)
                        (lambda (element context root)
                          (regexp-replace* rxpattern element replacement))]
                       [(symbol? replacement)
                        (let ([str (symbol->string replacement)])
                          (lambda (element context root)
                            (regexp-replace* rxpattern element str)))]
                       [(procedure? replacement)
                        (lambda (element context root)
                          (regexp-replace* rxpattern element (lambda (lst) (replacement (car lst)))))]
                       [else
                        (error "Invalid replacement" replacement)])])
          ((sxml:modify (list "//text()" proc))
           page)))))

;;; Procedure:
;;;   page-add-to-end
;;; Parameters:
;;;   page, a page
;;;   element, an xexp
;;; Purpose:
;;;   Add element to the end of the body of page
(define page-add-to-end
  (lambda (page element)
    ((sxml:modify (list "//body"
                        'insert-into
                        element))
     page)))

;;; Procedure:
;;;   update-attribute
;;; Parameters:
;;;   xexp, an xexp
;;;   parameter, a symbol
;;;   replacement, a procedure or string
;;; Purpose:
;;;   Update the given parameter of the xexp, either by applying
;;;   replacement or by using replacement.
;;; Produces:
;;;   updated, an xexp
(define update-attribute
  (lambda (xexp parameter replacement)
    (let ([new-attributes
           (let kernel ([attributes (cdr (extract-attributes xexp))])
             (cond
               [(null? attributes)
                (if (string? replacement)
                    (list (list parameter replacement))
                    null)]
               [(eq? parameter (caar attributes))
                (let ([value (if (null? (cdar attributes))
                                 ""
                                 (cadar attributes))])
                  (cons (list parameter (if (procedure? replacement)
                                            (replacement value)
                                            replacement))
                        (cdr attributes)))]
               [else
                (cons (car attributes)
                      (kernel (cdr attributes)))]))])
      (cons (car xexp)
            (cons (cons '@ new-attributes)
                  (if (has-attributes? xexp)
                      (cddr xexp)
                      (cdr xexp)))))))

;;; Procedure:
;;;   update-urls
;;; Parameters:
;;;   xexp, an xexp expression
;;;   current-url, a string or URL
;;; Purpose:
;;;   Transform all the relative URLs to be absolute.
;;; Produces:
;;;   updated, an xexp expression
(define update-urls
  (lambda (xexp current-url)
    (let* ([url (if (string? current-url)
                    (string->url current-url)
                    current-url)]
           [fix (lambda (relative)
                  (url->string (combine-url/relative url relative)))]
           [fixer (lambda (attribute)
                    (lambda (element context root)
                      (update-attribute element attribute fix)))])
      ((sxml:modify (list "//a" (fixer 'href))
                    (list "//img" (fixer 'src))
                    (list "//link" (fixer 'href))
                    (list "//script" (fixer 'src)))
       xexp))))

;;; Procedure:
;;;   sxpath-kernel
;;; Parameters:
;;;   path, an XPath as a string
;;;   xexp, an xexp expressions
;;;   modify, the result of sxml:modify
;;; Purpose:
;;;   Modify the xexp as described.
;;; Produces:
;;;   result, an xexp expression
(define sxpath-kernel
  (lambda (path xexp . instructions)
    (remove-top
     ((sxml:modify (cons path instructions))
      (add-top xexp)))))
    
;;; Purpose:
;;;   
;;; Procedure:
;;;   sxpath-delete
;;; Parameters:
;;;   path, an XPath as a string
;;;   xexp, an xexp expression
;;; Purpose:
;;;   Delete all of the elments that meet path
;;; Produces:
;;;   result, an xexp expression
(define sxpath-delete
  (lambda (path xexp)
    (sxpath-kernel path xexp 'delete)))

;;; Procedure:
;;;   sxpath-remove
;;; Parameters:
;;;   path, an XPath as a string
;;;   sexp, an sexp expression
;;; Purpose:
;;;   Delete the tags at the root of the elements marked by path.
;;; Produces:
;;;   result, an xexp exprssion
(define sxpath-remove
  (lambda (path xexp)
    (sxpath-kernel path xexp 'delete-undeep)))

;;; Procedure:
;;;   sxpath-replace
;;; Parameters:
;;;   path, an XPath as a string
;;;   xexp, an xexp expression
;;;   transform, a unary procedure
;;; Purpose:
;;;   Create a new xexp expression by appling transform to each element in
;;;   xexp described by path.
;;; Produces:
;;;   result, an xexp expression
(define sxpath-replace
  (lambda (path xexp transform)
    (sxpath-kernel path
                   xexp
                   (lambda (element context root)
                     (transform element)))))
    
(define sxpath-replace-old
  (lambda (path xexp transform)
    (remove-top
     ((sxml:modify (list path
                         ; sxml:modify expects a three parameter proc;
                         ; we expect a one-parameter proc.
                         (lambda (element context root)
                           (transform element))))
      (add-top xexp)))))

; +-------------------------+----------------------------------------
; | Miscellaneous utilities |
; +-------------------------+

;;; Procedure:
;;;   has-attributes?
;;; Parameters:
;;;   xexp, an xexp expression
;;; Purpose:
;;;   Determine if xexp appears to have attributes
;;; Produces:
;;;   appears-to-have-attributes?, a Boolean
(define has-attributes?
  (lambda (xexp)
    (and (pair? xexp)
         (not (null? (cdr xexp)))
         (attributes? (cadr xexp)))))

;;; Procedure:
;;;   attributes?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determines if val appears to represent a set of attributes.
;;; Produces:
;;;   appears-to-be-attributes?, a Boolean
(define attributes?
  (lambda (val)
    (and (pair? val)
         (eq? '@ (car val)))))

;;; Procedure:
;;;   extract-attributes
;;; Parameters:
;;;   xexp, an xexp expression
;;; Purpose:
;;;   Extract the attributes from the given xexp.
;;; Produces:
;;;   attributes, a list
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   * If (has-attributes? xexp), attributes is the attributes of xexp.
;;;   * Otherwise, attributes is the empty list of attributes
(define extract-attributes
  (lambda (xexp)
    (if (has-attributes? xexp)
        (cadr xexp)
        '(@))))

;;; Procedure:
;;;   extract-contents
;;; Parameters:
;;;   xexp, an xexp expressions
;;; Purpose:
;;;   Extract the contents of an xexp (everything but the tag and attributes)
;;; Produces:
;;;   contents, a list of xexp expressions
(define extract-contents
  (lambda (xexp)
    (if (pair? xexp)
        (if (has-attributes? xexp)
            (cddr xexp)
            (cdr xexp))
        xexp)))
    
;;; Procedure:
;;;   double-class
;;; Parameters:
;;;   xexp, an xexp expression
;;; Purpose:
;;;   Find and print all the elements in xexp that have two class tags
;;; Produces:
;;;   [Nothing; called for the side effect]
(define double-class
  (lambda (xexp)
    (when (pair? xexp)
      (when (< 1 (tally (lambda (att) (eq? 'class (car att))) (cdr (extract-attributes xexp))))
        (write xexp)
        (newline)
        (newline))
      (for-each double-class (extract-contents xexp)))))

;;; Procedure:
;;;   merge-class-attributes
;;; Parameters:
;;;   attributes, a standard attribute list
;;; Purpose:
;;;   Merge all the class attributes in the list
;;; Produces:
;;;   new-attributes, a standard attribute list
;;; Preconditions:
;;;   attributes has the form (@ (att1 val1) (att2 val2) ...)
;;; Postconditions:
;;;   * new-attributes has the form (@ (att1 val1) (att2 val2) ...)
;;;   * If (att val) is in attributes and att is not class, (att val)
;;;     is in new-attributes.
;;;   * If ('class val) is in attributes then there is a class attribute
;;;     in new-attributes and val is in it.
(define merge-class-attributes
  (lambda (attributes)
    (let ([class-attributes
           (filter (lambda (att) (eq? (car att) 'class)) (cdr attributes))]
          [other-attributes
           (filter (lambda (att) (not (eq? (car att) 'class))) (cdr attributes))])
      (if (or (null? class-attributes)
              (null? (cdr class-attributes)))
          attributes
          (cons '@
                (cons (list 'class
                            (reduce (lambda (a b) (string-append a " " b))
                                    (map cadr class-attributes)))
                      other-attributes))))))

;;; Procedure:
;;;   fix-multiple-classes
;;; Parameters:
;;;   xexp, an xexp expression
;;; Purpose:
;;;   Fix the elements in xexp that have more than one class attribute.
;;; Produces:
;;;   fixed-xexp, an xexp expression
;;; Pondering:
;;;   Officially, an xexp expression should not have two class attributes.
;;;   Unfortunately, many Web pages do have multiple class attributes.
;;;   This procedure attempts to address that deficiency.
(define merge-multiple-classes
  (lambda (xexp)
    (if (not (pair? xexp))
        xexp
        (if (has-attributes? xexp)
            (cons (car xexp)
                  (cons (merge-class-attributes (cadr xexp))
                        (map merge-multiple-classes (cddr xexp))))
            (cons (car xexp)
                  (map merge-multiple-classes (cdr xexp)))))))

;;; Procedure:
;;;   regexp-pieces
;;; Parameters:
;;;   re, a regexp
;;;   str, a string
;;; Purpose:
;;;   Split the string at every copy of re, generating a list of strings
;;;   of alternating matching and non-matching text.
;;; Produces:
;;;   pieces, a list of strings
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (apply string-append pieces) = str
;;;   In most cases, (regexp-match? re (list-ref pieces i)) holds
;;;    for all reasonable even i and fails to hold or all reasonable
;;;    odd i, or vice versa
;;; Problems:
;;;   Does less well with strings in which the pattern can appear
;;;   twice in a row.
(define (regexp-pieces regexp str)
  (let ([add-substring
         (lambda (start finish lst)
           (if (< start finish)
               (cons (substring str start finish) lst)
               lst))]
        [len (string-length str)])
    (let kernel ([cur 0]
                 [positions (regexp-match-positions* regexp str)]
                 [substrings null])
      (if (null? positions)
          (reverse (add-substring cur len substrings))
          (let* ([front (caar positions)]
                 [back (cdar positions)])
            (kernel back
                    (cdr positions)
                    (add-substring front back
                                   (add-substring cur front
                                                  substrings))))))))

;;; Procedure:
;;;   add-top
;;; Parameters:
;;;   sexp, an sexp expression
;;; Purpose:
;;;   Adds *TOP*, if necessary.
;;; Produces:
;;;   topped, an sexp expression
;;; Philosophy:
;;;   Some of the XPath/XSLT procedures require this
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (car topped) = '*TOP*
(define add-top
  (lambda (sexp)
    (if (and (pair? sexp) (eq? '*TOP* (car sexp)))
        sexp
        (list '*TOP* sexp))))

;;; Procedure:
;;;   remove-top
;;; Parameters:
;;;   sexp, an sexp expression
;;; Purpose:
;;;   Remove the *TOP* that may have been added.
;;; Produces:
;;;   result, an sexp expression
;;; Philosophy:
;;;   "undoes" add-top
(define remove-top
  (lambda (sexp)
    (if (and (pair? sexp) (eq? (car sexp) '*TOP*))
        (if (and (pair? (cdr sexp)) (string? (cadr sexp)))
            (cadr sexp)
            (cdr sexp))
        sexp)))

;;; Procedure:
;;;   filter-out-whitespace
;;; Parameters:
;;;   xexp, an xexp expression
;;; Purpose:
;;;   Remove any strings from xexp that consist of only whitespace.
;;; Produces:
;;;   result, an xexp expression
(define filter-out-whitespace
  (lambda (xexp)
    (cond
      [(pair? xexp)
       (map filter-out-whitespace (filter (lambda (exp)
                                            (or (not (string? exp))
                                                (not (regexp-match-exact? "[ \t\n]*" exp))))
                                          xexp))]
      [else
       xexp])))

;;; Procedure:
;;;   strip-newlines
;;; Parameters:
;;;   xexp, an xexp epression
;;; Purpose:
;;;   Remove most of the newlines from the given document.
;;; Produces:
;;;   result, an xexp expression
(define strip-newlines
  (lambda (xexp)
    (cond
      [(string? xexp)
       (regexp-replace* "^\n|\n$" xexp "")]
      [(pair? xexp)
       (map strip-newlines xexp)]
      [else
       xexp])))