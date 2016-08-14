#lang scheme/base
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSEQ.PLT 
;; A Parser Combinator library.
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; csv.ss - a customizable csv reader 
;; yc 12/31/2009 - first version 
(require "../main.ss"
         )

;; creating a delimiter-based string. 
(define (delim-string delim)
  (seq s <- (zero-many (choice (escaped-char #\\ delim) 
                               (char-not-in (list delim #\return #\newline))))
       (return (list->string s))))

;; csv-string 
;; combine between quoted string and delimited string 
(define (csv-string delim)
  (choice quoted-string (delim-string delim)))

;; csv-record 
;; reads a list of csv-strings by skipping over the delimiters
(define (csv-record delim)
  (delimited (csv-string delim) (char= delim)))

;; csv-table
;; reads over a csv-table 
(define (csv-table delim) 
  (delimited (csv-record delim) newline))

;; make-csv-reader
;; creates a csv-reader based on the delim... 
(define (make-csv-reader delim)
  (make-reader (csv-table delim))) 

;; contract 
(provide make-csv-reader) 
