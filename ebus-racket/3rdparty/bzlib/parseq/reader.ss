#lang scheme/base
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSEQ.PLT 
;; A Parser Combinator library.
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader.ss - move make-reader & Reader/c here 
;; yc 1/18/2010 - first version 
;; yc 1/21/2010 - make-reader to take on additional default params 
(require "depend.ss"
         "input.ss"
         "primitive.ss"
         "combinator.ss"
         (prefix-in c: scheme/contract)
         )
;; use this to create a reader that will read the bytes if the parse succeeds.
(define (make-reader parser #:sof? (sof? #t) #:eof? (eof? #t) #:default (default #f)) 
  (lambda (in #:sof? (sof? sof?) #:eof? (eof? eof?) #:default (default default)) 
    (let-values (((v in)
                  ((seq (if sof? SOF (return #t))
                        v <- parser 
                        (if eof? EOF (return #t)) 
                        (return v)) (make-input in))))
      (unless (failed? v) (read-bytes* in))
      (if (failed? v)
          default 
          v))))

(define Reader/c (c:->* (Input/c)
                      (#:sof? boolean? #:eof? boolean? #:default c:any/c) 
                      c:any))
(provide Reader/c)
(c:provide/contract
 (make-reader (c:->* (Parser/c)
                   (#:sof? boolean? #:eof? boolean? #:default c:any/c)
                   Reader/c))
 )

