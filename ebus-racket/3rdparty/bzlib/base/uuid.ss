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
;; uuid.ss  - provide uuid object (currently wrapping over zitterbewegung/uuid-v4) 
;; yc 9/11/2009 - first version 
(require (prefix-in z: "../../zitterbewegung/uuid/uuid-v4.ss")
         "base.ss"
         scheme/list
         scheme/string
         scheme/contract
         ;; (planet vyzo/crypto/util)
         )

(define (bytes->integer bytes)
  (define (helper rest num)
    (if (null? rest) num
        (helper (cdr rest) (+ (* num 255) (car rest)))))
  (helper (bytes->list bytes) 0))

(define (bytes->hex bytes)
  (define (helper rest acc)
    (if (null? rest) (list->string (map hex-byte->char (reverse acc)))
        (helper (cdr rest) 
                (let-values (((quotient remainder)
                              (quotient/remainder (car rest) 16)))
                  (list* remainder quotient acc)))))
  (helper (bytes->list bytes) '()))

;; *uuid structure - representing UUID, and holds bytes format... 
(define-struct *uuid (bytes)
  #:property prop:custom-write 
  (lambda (u out write?)
    (display (format "#<uuid:~a>" (uuid->string u)) out))
  #:property prop:equal+hash 
  (list (lambda (u1 u2 sub?)
          (bytes=? (*uuid-bytes u1) (*uuid-bytes u2)))
        (lambda (u recur)
          (bytes->integer (*uuid-bytes u)))
        (lambda (u recur)
          (bytes->integer (*uuid-bytes u)))))

(define (uuid-time-low u)
  (integer-bytes->integer (subbytes (*uuid-bytes u) 0 4) #f #t))

(define (uuid-time-mid u)
  (integer-bytes->integer (subbytes (*uuid-bytes u) 4 6) #f #t))

(define (uuid-time-high u)
  (integer-bytes->integer (subbytes (*uuid-bytes u) 6 8) #f #t))

(define (uuid-clock-high u)
  (integer-bytes->integer (bytes-append (list->bytes (list 0)) 
                                        (subbytes (*uuid-bytes u) 8 9)) #f #t))

(define (uuid-clock-low u)
  (integer-bytes->integer (bytes-append (list->bytes (list 0)) 
                                        (subbytes (*uuid-bytes u) 9 10)) #f #t))

(define (uuid-node u)
  (integer-bytes->integer (bytes-append (list->bytes (list 0 0)) 
                                        (subbytes (*uuid-bytes u) 10 16)) #f #t))

(define (uuid->string u (dash? #t))
  (define (sub start end)
    (subbytes (*uuid-bytes u) start end)) 
  (if (not dash?)
      (bytes->hex (*uuid-bytes u))
      (string-join (map (lambda (b)
                          (bytes->hex b)) 
                        (list (sub 0 4) (sub 4 6) (sub 6 8) (sub 8 10) (sub 10 16)))
                   "-")))


(define (uuid-string? u)
  (and (string? u)
       (regexp-match #px"^(?i:([0-9a-f]{,8})-?([0-9a-f]{,4})-?([0-9a-f]{,4})-?([0-9a-f]{,4})-?([0-9a-f]{,12}))$" u)))

(define (uuid-symbol? u)
  (and (symbol? u)
       (uuid-string? (symbol->string u))))

(define (uuid-bytes? u)
  (and (bytes? u) 
       (= (bytes-length u) 16)))

;; an uuid should be one of the following:
;; struct of *uuid
;; 16-bytes byte string. 
;; a string of 32 or 36 hex chars. 
(define (uuid? u)
  (or (*uuid? u)
      (uuid-bytes? u)
      (uuid-string? u)))

(define (make-uuid (u (symbol->string (z:make-uuid))))
  (cond ((*uuid? u)
         (make-*uuid (*uuid-bytes u)))
        ((uuid-bytes? u)
         (make-*uuid u))
        (else
         (uuid-string->uuid u))))

(define (hex-byte->char h)
  (case h
    ((0) #\0)
    ((1) #\1)
    ((2) #\2)
    ((3) #\3)
    ((4) #\4)
    ((5) #\5)
    ((6) #\6)
    ((7) #\7)
    ((8) #\8)
    ((9) #\9)
    ((10) #\a)
    ((11) #\b)
    ((12) #\c)
    ((13) #\d)
    ((14) #\e)
    ((15) #\f)
    (else (error 'hex-byte->char "Not an hex byte: ~a" h))))

(define (hex-char->integer c)
  (case c
    ((#\1) 1)
    ((#\2) 2)
    ((#\3) 3)
    ((#\4) 4)
    ((#\5) 5)
    ((#\6) 6)
    ((#\7) 7)
    ((#\8) 8)
    ((#\9) 9)
    ((#\0) 0)
    ((#\a #\A) 10)
    ((#\b #\B) 11)
    ((#\c #\C) 12)
    ((#\d #\D) 13)
    ((#\e #\E) 14)
    ((#\f #\F) 15)
    (else (error 'hex-char->integer "char ~a out of range" c))))

(define (hex-char? c)
  (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\A #\b #\B #\c #\C #\d #\D #\e #\E #\f #\F)))

(define (hex-chars->byte chars)
  (define (helper rest num)
    (if (null? rest)
        num
        (helper (cdr rest) (+ (* 16 num) (hex-char->integer (car rest))))))
  (helper chars 0))

(define (hex-string->bytes h)
  (define (helper rest acc)
    (cond ((null? rest) (reverse acc))
          ((null? (cdr rest)) ;; wrong
           (error 'hex-string->bytes "Uneven # of hexdecimal strings: ~a" h))
          (else
           (helper (cddr rest)
                   (cons (hex-chars->byte (list (car rest) (cadr rest)))
                         acc)))))
  (helper (string->list h) '()))

(define (uuid-string->uuid uuid)
  (make-*uuid (list->bytes (flatten (map hex-string->bytes (cdr (uuid-string? uuid)))))))

;; how quickly can all the generation take? 
;; it seems that 
(provide/contract 
 (make-uuid (->* ()
                 (uuid?)
                 *uuid?))
 (uuid->string (->* (*uuid?)
                    (boolean?) 
                    string?))
 (rename *uuid-bytes uuid->bytes (-> *uuid? bytes?))
 (uuid-string? (-> any/c any))
 (uuid-bytes? (-> any/c any))
 (uuid-time-low (-> *uuid? number?))
 (uuid-time-mid (-> *uuid? number?))
 (uuid-time-high (-> *uuid? number?))
 (uuid-clock-low (-> *uuid? number?))
 (uuid-clock-high (-> *uuid? number?))
 (uuid-node (-> *uuid? number?))
 (uuid? (-> any/c any))
 (bytes->hex (-> bytes? string?))
 (bytes->integer (-> bytes? number?))
 )

