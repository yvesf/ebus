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
;; text.ss - basic "text" (or string) service. 
;; yc 9/8/2009 - first version 
;; yc 2/1/2010 - adding the ability to extend the behavior of the string function... 
(require "base.ss" 
         scheme/string
         "registry.ss"
         scheme/function
         scheme/contract
         )

(define default->string (curry format "~a"))

(define string-converter-table (make-cond-registry '())) 

(define (string-converter-ref obj)
  (registry-ref string-converter-table obj default->string))

(define (string-converter-set! type? converter)
  (registry-set! string-converter-table type? converter))

(define (string-converter-del! type?)
  (registry-del! string-converter-table type?));;

(define (stringify* arg . args)
  (stringify (cons arg args)))

(define (any->string v)
  (cond ((string? v) v)
        (else 
         ((string-converter-ref v) v))))

(define (stringify args)
  (string-join (map any->string args) ""))

(provide/contract 
 (stringify* (->* (any/c)
                 ()
                 #:rest (listof any/c)
                 string?))
 (stringify (-> (listof any/c) string?))
 ;;(string-converter-table registry?)
 (string-converter-ref (-> any/c any))
 (string-converter-set! (-> procedure? procedure? any))
 (string-converter-del! (-> procedure? any))
 (any->string (-> any/c string?)) 
 (rename stringify* any*->string (->* (any/c)
                                      () 
                                      #:rest (listof any/c)
                                      string?))
 (rename stringify any/list->string (-> (listof any/c) string?))
 )

(provide (all-from-out scheme/string))