#lang scheme/base
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSEQ.PLT 
;; A Parser Combinator library.
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input.ss - holds the abstraction of the input object... 
;; yc 12/31/2009 - first version 
;; yc 1/8/2009 - fix build-input & Input/c 
(require scheme/contract)
;; state 
;; the struct that abstracts the input 
;; currently this holds an input-port + the position on the port
;; in the future this can be used to hold string, list, vector, etc. 
(define-struct input (source pos) #:prefab) 

;; input
;; an utility for converting source into input state.
(define (build-input v (pos 0))
  (define (helper v)
    (cond ((input-port? v) v)
          ((string? v) (open-input-string v)) 
          ((bytes? v) (open-input-bytes v))))
  (if (input? v) 
      (new-input v pos)
      (make-input (helper v) pos))) 

;; new-input 
;; make a new input based on the old input and a new position... 
(define (new-input input incr)
  (make-input (input-source input) 
              (+ incr (input-pos input))))

;; peek-bytes* 
;; return a funtion that will make a particular amount of reading based on 
;; the requested size... 
(define (peek-bytes* size) 
  (lambda (in)
    (peek-bytes size (input-pos in) (input-source in))))

;; peek-string* 
;; return a function that will read a particular size of string... 
;; this can fail since it is expected to be using utf-8 as the input size... 
(define (peek-string* size) 
  (lambda (in) 
    (peek-string size (input-pos in) (input-source in))))

;; peek-byte*
;; peek a single byte 
(define (peek-byte* in)
  (peek-byte (input-source in) (input-pos in)))

;; peek-char*
;; peek a single char
(define (peek-char* in)
  (peek-char (input-source in) (input-pos in)))

;; read-bytes* 
;; read out the bytes based on the size of the input... 
(define (read-bytes* in)
  (read-bytes (input-pos in) (input-source in)))

(define Input/c (or/c input? bytes? string? input-port?))

(define Parser/c (-> Input/c (values any/c Input/c)))

(provide input
         input?
         input-source
         input-pos
         (rename-out (build-input make-input))
         new-input
         peek-bytes*
         peek-string*
         peek-byte*
         peek-char*
         read-bytes*
         Input/c
         Parser/c 
         )