#lang scheme/base
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSEQ.PLT 
;; A Parser Combinator library.
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic.ss - a set of basic parsers 
;; yc 12/31/2009 - first version 
;; yc 7/7/2010 - updating real-number to also handle exponents. 

(require "depend.ss"
         "primitive.ss"
         "combinator.ss"
         "input.ss"
         )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; char parsers.
;; digit 
(define digit (char-between #\0 #\9)) 

;; not-digit 
(define not-digit (char-not-between #\0 #\9)) 

;; lower-case 
(define lower-case (char-between #\a #\z))

;; upper-case
(define upper-case (char-between #\A #\Z)) 

;; alpha
(define alpha (choice lower-case upper-case))

;; alphanumeric
(define alphanumeric (choice alpha digit)) 

;; hexdecimal parser 
(define hexdecimal (char-in '(#\a #\b #\c #\d #\e #\f
                                  #\A #\B #\C #\D #\E #\F 
                                  #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

;; whitespace 
(define whitespace (char-in '(#\space #\return #\newline #\tab #\vtab)))

(define not-whitespace (char-not-in '(#\space #\return #\newline #\tab #\vtab)))

;; ascii 
(define ascii (char-between (integer->char 0) (integer->char 127)))

;; word = a-zA-Z0-9_ 
(define word (choice alphanumeric (char= #\_)))

;; not-word
(define not-word (char-when (lambda (c) 
                              (not (or (char<=? #\a c #\z)
                                       (char<=? #\A c #\Z)
                                       (char<=? #\0 c #\9) 
                                       (char=? c #\_))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; number parsers. 

;; signed 
(define sign (zero-one (char= #\-) #\+))

;; natural 
(define natural (one-many digit)) 

;; decimal 
;; there is a bug - anything fails in seq should automatically fail the whole thing... 
(define decimal (seq number <- (zero-many digit)
                     point <- (char= #\.)
                     decimals <- natural 
                     (return (append number (cons point decimals)))))

(define (hexdecimals->number hexes)
  (define (hex->num hex)
    (- (char->integer hex) 
       (char->integer (case hex
                        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) #\0)
                        ((#\a #\b #\c #\d #\e #\f) #\a)
                        ((#\A #\B #\C #\D #\E #\F) #\A)))
       (- (case hex 
            ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) 0)
            ((#\a #\b #\c #\d #\e #\f) 10)
            ((#\A #\B #\C #\D #\E #\F) 10)))))
  (define (helper rest total) 
    (if (null? rest) 
        total
        (helper (cdr rest) (+ (hex->num (car rest)) (* total 16)))))
  ;;(trace helper)
  ;;(trace hex->num)
  (helper hexes 0))

(define hexdecimals (seq num <- (zero-many hexdecimal)
                         (return (hexdecimals->number num))))

;; positive 
(define positive (choice decimal natural)) 

;; signed (number) 
(define (make-signed parser)
  (seq +/- <- sign
       number <- parser 
       (return (cons +/- number)))) 

;; make-number 
(define (make-number parser)
  (seq n <- parser 
       (return (string->number (list->string n)))))

;; natural-number 
(define natural-number (make-number natural))

;; integer 
(define integer (make-number (make-signed natural))) 

;; positive-integer 
(define positive-number (make-number positive)) 

;; real-number (now handling exponents) 
(define real-number (make-number (choice (seq exp <- (make-signed positive)
                                              e <- (choice #\E #\e) 
                                              magenta <- (make-signed natural)
                                              (return (append exp (list e) magenta)))
                                         (make-signed positive) 
                                         )))

(define hexdecimal-number (make-number hexdecimals))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string parsers. 

;; escaped-char 
;; allows for an escaping sequence for a particular character... 
(define (escaped-char escape char (as #f)) 
  (seq (char= escape) 
       c <- (if (char? char) (char= char) char)
       (return (if as as c)))) 

;; e-newline 
(define e-newline (escaped-char #\\ #\n #\newline)) 

;; e-return 
(define e-return (escaped-char #\\ #\r #\return)) 

;; e-tab 
(define e-tab (escaped-char #\\ #\t #\tab)) 

;; e-backslash 
(define e-backslash (escaped-char #\\ #\\))

;; quoted 
;; a specific string-based bracket parser 
(define (quoted open close escape)
  (seq (char= open) 
       atoms <- (zero-many (choice e-newline 
                                   e-return 
                                   e-tab 
                                   e-backslash 
                                   (escaped-char escape close) 
                                   (char-not-in  (list close #\\)))) 
       (char= close)
       (return atoms)))

;; make-quoted-string 
;; a simplification for creating a string parser 
(define (make-quoted-string open (close #f) (escape #\\)) 
  (seq v <- (quoted open (if close close open) escape)
       (return (list->string v))))

;; single-quoted-string 
;; parse a string with single quotes 
(define single-quoted-string (make-quoted-string #\'))

;; double-quoted-string 
;; parse a string with double quotes 
(define double-quoted-string (make-quoted-string #\"))

;; quoted-string 
;; choosing between single and double quotes 
(define quoted-string 
  (choice single-quoted-string double-quoted-string))

;; whitespaces 
;; parsing out all whitespaces together... 
(define whitespaces (zero-many whitespace))

;; newline 
(define newline 
  (choice (seq r <- (char= #\return) 
               n <- (char= #\newline)
               (return (list r n)))
          (char= #\return)
          (char= #\newline)))

(provide (all-defined-out)) 
