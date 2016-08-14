#lang scheme
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSEQ.PLT 
;; A Parser Combinator library.
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql.ss - parsing the create table statement  
;; yc 1/5/2010 - first version 
(require "../main.ss"
         mzlib/defmacro
         (for-syntax scheme/base
                     scheme/match
                     )
         (planet bzlib/base) 
         ) 

(define sql-identifier 
  (seq c <- alpha 
       rest <- (zero-many word) 
       (return (string->symbol 
                (string-downcase (list->string (cons c rest)))))))

(define create-table-def
  (tokens-ci "create" "table"
             name <- sql-identifier 
             clauses <- (bracket #\( 
                                 (delimited clause-def #\,)  
                                 #\)) 
             (return (cons name clauses))))

(define clause-def 
  (choice primary-key-def foreign-key-def column-def)) 

;; making things without order would be quite a difficult combinator. 
;; basically we need to try each of the combinator, and then as we have the binding
;; make sure it is returned in a way that can easily be identified... 
;; for example, the first 
(define (self-and-value parser) 
  (seq v <- parser 
       (return (cons parser v)))) 

(define (one-of-each parsers defaults) 
  ;; we need to try each one, and then figure out the *rest* that weren't matched
  ;; continue until we are either out of the stream or out of the combinator... 
  ;; at any time there is anything that none of them matches then we will be in trouble... 
  (define (each-helper parsers) 
    (one-of (map self-and-value parsers))) 
  (define (sort-helper acc parsers defaults) 
    (map (lambda (v default) 
           (if (pair? v) 
               (cdr v) 
               default)) 
         (map (lambda (parser) 
                (assf (lambda (p) 
                        (eq? p parser)) 
                      acc)) 
              parsers)
         defaults))
  ;; if all of them failed @ the next position, then we need to offer 
  ;; default values for the remainder of the parsers!!! 
  ;; this is where it is *interesting!!!... 
  ;; in such case we want to have a chance to work on the *fail* clause... 
  ;; this is hmm.... 
  (define (helper rest acc) 
    (bind (each-helper rest) 
          (lambda (v) 
            (if (succeeded? v) 
                (let ((rest (remove (car v) rest))) 
                  (if (null? rest) 
                      (return (sort-helper acc parsers defaults))
                      (helper rest (cons v acc))))
                (return (sort-helper acc parsers defaults))))))
  (helper parsers '())) 

(define-syntax one-of-each* 
  (syntax-rules () 
    ((~ (parser default) ...) 
     (one-of-each (list parser ...) (list default ...)))))

(define column-def 
  (tokens name <- sql-identifier
          attrs <- (one-of-each* (type-def 'text)
                                 (nullability 'null) 
                                 (inline-primary-key #f) 
                                 (inline-foreign-key #f))
          (return (cons name attrs))))

(define nullability 
  (choice (tokens-ci "null" (return 'null)) 
          (tokens-ci "not" "null" (return 'not-null))))

(define type-def 
  (seq type <- (choice (string-ci= "int")
                       (string-ci= "integer") 
                       (string-ci= "float")
                       (string-ci= "text"))
       (return (string->symbol type))))

(define inline-primary-key 
  (tokens-ci "primary" "key" (return 'pkey)))
;; (trace inline-primary-key) 

(define sql-identifiers/paren 
  (bracket #\( (delimited sql-identifier #\,) #\))) 

(define inline-foreign-key 
  (tokens-ci "foreign" "key"  
             (zero-one (string-ci= "references") "references") 
             table <- sql-identifier 
             (zero-one (string-ci= "on") "on") 
             columns <- sql-identifiers/paren 
             (return `(foreign-key ,table ,columns))))

(define primary-key-def 
  (tokens-ci "primary" "key" 
             name <- (zero-one sql-identifier #f) 
             columns <- sql-identifiers/paren  
             (return `(primary-key ,name ,columns))))

(define foreign-key-def 
  (tokens-ci "foreign" "key" 
             name <- (zero-one sql-identifier #f) 
             columns <- sql-identifiers/paren 
             (string-ci= "references")
             table <- sql-identifier 
             (zero-one (string-ci= "on") "on") 
             fk-columns <- sql-identifiers/paren 
             (return `(foreign-key ,name ,columns ,table ,fk-columns))))

;; (provide create-table-def) 
(define sql-def (choice create-table-def))

(define read-sql (make-reader sql-def)) 

(provide read-sql) 
