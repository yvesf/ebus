#lang scheme/base
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASE.plt - common routines that are shared by all other bzlib modules 
;; 
;; in a way, base.plt is the most fundamental module of the whole bzlib stack
;; and as such it also is the lowest level code.  We are not likely to 
;; fix the code any time soon, and hence any of the functions here are 
;; explicitly likely to be obsoleted or moved elsewhere. 
;; 
;; Proceed with caution. 
;; 
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; synatx.ss - syntax helpers
;; yc 10/19/2009 - first version 
;; yc 7/6/2010 - fixed syntax-identifier-append 
(require (for-syntax scheme/base)
          syntax/stx scheme/string mzlib/trace
          scheme/contract
          )

(define (syntax-map proc stx-lst)
  (syntax-case stx-lst ()
    (() #'())
    ((id . rest) 
     #`(#,(proc #'id) . #,(syntax-map proc #'rest)))))

(define (syntax-identifier-append arg #:stx (stx #f) . args)
  (define (get-first-syntax lst)
    (define (helper lst)
      (cond ((null? lst) (error 'syntax-identifier-append "no stx for context"))
            ((syntax? (car lst)) (car lst))
            (else (helper (cdr lst)))))
    (if (not stx) (helper lst) stx))
  (define (->string x)
    (cond ((syntax? x) (->string (syntax->datum x)))
          (else (format "~a" x))))
  (define (helper args)
    (datum->syntax (get-first-syntax args)
                   (string->symbol (string-join (map ->string args) ""))))
  (helper (cons arg args)))

(define (syntax-id-part? stx)
  (define (helper part)
    (or (symbol? part) (bytes? part) (string? part) (number? part)))
  (or (and (syntax? stx)
           (helper (syntax->datum stx)))
      (helper stx)))

(provide/contract 
 (syntax-map (-> (-> any/c any) stx-pair? any))
 (syntax-identifier-append (->* (syntax-id-part?)
                                (#:stx syntax?)
                                #:rest (listof syntax-id-part?)
                                syntax?))
 )

(provide (all-from-out syntax/stx))

