#lang scheme/base
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSEQ.PLT 
;; A Parser Combinator library.
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json.ss - a parser for the json format 
;; yc 1/5/2010 - first version 
;; yc 7/76/2010 - updated json-string to handle single quotes. 
(require "../main.ss"
         )

(define hex-digit (seq d <- (choice digit #\a #\b #\c #\d #\e #\f
                                    #\A #\B #\C #\D #\E #\F)
                       (return (case d 
                                 ((#\0) 0)
                                 ((#\1) 1)
                                 ((#\2) 2) 
                                 ((#\3) 3)
                                 ((#\4) 4)
                                 ((#\5) 5)
                                 ((#\6) 6) 
                                 ((#\7) 7)
                                 ((#\8) 8)
                                 ((#\9) 9) 
                                 ((#\a #\A) 10)
                                 ((#\b #\B) 11)
                                 ((#\c #\C) 12)
                                 ((#\d #\D) 13)
                                 ((#\e #\E) 14)
                                 ((#\f #\F) 15)))))

(define (hex->char h) 
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
    ((15) #\f)))


(define (hexes->char hexes) 
  (integer->char (hexes->integer hexes))) 

(define (char->hexes c) 
  (integer->hexes (char->integer c))) 

(define (char->hex-chars c)
  (map hex->char (char->hexes c))) 

(define (hexes->integer hexes)
  (define (helper rest acc) 
    (cond ((null? rest) acc) 
          (else
           (helper (cdr rest) (+ (* acc 16) (car rest))))))
  (helper hexes 0)) 

(define (integer->hexes i) 
  (define (helper q acc) 
    (if (= q 0) 
        acc 
        (let-values (((q r) 
                      (quotient/remainder q 16))) 
          (helper q (cons r acc)))))
  (helper i '()))

(define unicode-char
  (seq #\\ #\u 
       code <- (repeat hex-digit 4 4) 
       (return (hexes->char code)))) 

(define (json-string/inner quote)
  (zero-many (choice e-newline 
                     e-return 
                     e-tab 
                     e-backslash 
                     (escaped-char #\\ quote) 
                     (escaped-char #\\ #\/) 
                     (escaped-char #\\ #\\) 
                     (escaped-char #\\ #\b #\backspace) 
                     (escaped-char #\\ #\f #\page) 
                     unicode-char
                     (char-not-in  (list quote 
                                         #\newline 
                                         #\return 
                                         #\tab 
                                         #\\ 
                                         #\backspace 
                                         #\page))
                     )))

(define json-string
  (choice (seq #\' atoms <- (json-string/inner #\') #\'
               (return (list->string atoms)))
          (seq #\" atoms <- (json-string/inner #\") #\" 
               (return (list->string atoms)))))

(define json-array (tokens v <- (bracket/delimited #\[ json-value #\, #\])
                           (return (list->vector v))))

(define json-object (tokens v <- (bracket/delimited #\{ json-pair #\, #\}) 
                            (return (make-immutable-hash v))))

(define json-pair (tokens key <- (choice json-string 
                                         (seq c <- alpha 
                                              lst <- (zero-many alphanumeric)
                                              (return (list->string (cons c lst)))))
                          #\: 
                          value <- json-value 
                          (return (cons key value)))) 

(define json-literal (choice (tokens "true" (return #t)) 
                             (tokens "false" (return #f))
                             (tokens "null" (return '())) 
                             ))

(define json-value (choice json-literal json-array json-object real-number json-string)) 

(define read-json (make-reader json-value)) 

(provide read-json) 
