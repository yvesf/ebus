#lang scheme/base
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSEQ.PLT 
;; A Parser Combinator library.
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; util.ss - an utility module... the code might be moved out of here... 
;; yc 12/31/2009 - first version 

(require mzlib/etc
         )

;; the-number
;; makes a function that returns a particular number no matter what 
;; args are passed in
(define (the-number n)
  (lambda args n))

;; bits->byte
;; convert a list of bits into its corresponding byte (or integer...) 
;; note the byte can be greater than 255
(define (bits->byte bits)
  (define (->i bit)
    (case bit
      ((0 #f) 0)
      ((1 #t) 1)))
  (apply + 
         (map (lambda (bit exponent)
                (* (->i bit) (expt 2 exponent)))
              bits
              (reverse (build-list (length bits) identity)))))

;; byte->bits 
;; the reverse of converting byte to bits... 
(define (byte->bits b) 
  (define (helper q acc) 
    (cond ((= 0 q) acc)
          (else
           (let-values (((q r)
                         (quotient/remainder q 2)))
             (helper q (cons r acc))))))
  (helper b '()))

;; string-bytes/utf-8-length 
;; return the bytes length for a string (instead of character length) 
(define (string-bytes/utf-8-length s)
  (bytes-length (string->bytes/utf-8 s)))

(provide (all-defined-out)) 

