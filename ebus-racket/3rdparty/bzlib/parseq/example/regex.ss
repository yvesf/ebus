#lang scheme/base
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSEQ.PLT 
;; A Parser Combinator library.
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; regex.ss - a simple regular expression parser 
;; yc 1/1/2009 - first version 
(require "../main.ss"
         mzlib/trace
         )

;; sof = start of file
(define regex-sof (zero-one (char= #\^) #\$))

;; eof = end of file 
(define regex-eof (zero-one (char= #\$) #\^)) 

;; meta-chars - a list of meta characters 
(define regex-meta-chars '( #\. #\+ #\* #\? #\^ #\$ #\[ #\] #\( #\) #\{ #\} #\\))

;; digit = \\d 
(define regex-digit (seq "\\d" (return digit)))

;; not-digit = \\D 
(define regex-not-digit (seq "\\D" (return not-digit)))

;; word = \\w 
(define regex-word (seq "\\w" (return word)))

;; not-word = \\W 
(define regex-not-word (seq "\\W" (return not-word)))

;; whitespace = \\s 
(define regex-whitespace (seq "\\s" (return whitespace)))

;; not-whitespace = \\S 
(define regex-not-whitespace (seq "\\S" (return not-whitespace)))

;; any-char = . 
(define regex-any-char (seq #\. (return any-char))) 

;; literal = \\d | \\D | \\w | \\W | \\s | \\S | . | \n | \r | \t | \\ | other chars
(define regex-literal (choice regex-digit 
                              regex-not-digit 
                              regex-word 
                              regex-not-word 
                              regex-whitespace 
                              regex-not-whitespace 
                              regex-any-char 
                              (seq v <- (choice e-newline
                                                e-return
                                                e-tab
                                                (escaped-char #\\ any-char) 
                                                (char-not-in regex-meta-chars))
                                   (return (char= v))))) 

;; atom = literal | group | choice 
(define regex-atom (choice regex-literal
                           regex-group
                           regex-choice 
                           )) 

;; char-range = <lc>-<hc>, e.g., a-z 
(define regex-char-range (seq lc <- (char-not-in (cons #\- regex-meta-chars))
                              #\- 
                              hc <- (char-not-in (cons #\- regex-meta-chars))
                              (return `(,char-between ,lc ,hc))))

;; choice = [<char-range | literal>+]
(define regex-choice (seq #\[ 
                          literals <- (one-many (choice regex-char-range 
                                                     regex-literal))
                          #\] 
                          (return `(,one-of* ,@literals))))

;; group = (<atom>+) 
(define regex-group (seq #\( 
                         chars <- (one-many regex-atom)
                         #\) 
                         (return `(,sequence* ,@chars))))

;; regex combinators 
;; zero-one = <atom>?
(define regex-zero-one (seq v <- regex-atom 
                            #\? 
                            (return `(,zero-one ,v))))
;; zero-many = <atom>* 
(define regex-zero-many (seq v <- regex-atom 
                             #\* 
                             (return `(,zero-many ,v))))

;; one-many = <atom>+ 
(define regex-one-many (seq v <- regex-atom 
                            #\+ 
                            (return `(,one-many ,v))))

;; range = <atom>{min,max} | <atom>{times} 
(define regex-range (seq v <- regex-atom 
                         #\{ 
                         min <- (zero-one natural-number 0)
                         max <- (zero-one (seq #\,
                                               max <- (zero-one natural-number +inf.0)
                                               (return max)) 
                                          min)
                         #\} 
                         (return `(,repeat ,v ,min ,max))))

;; exp = sof ? <zero-one | zero-many | one-many | range | atom>* eof ? 
(define regex-exp (seq SOF 
                       sof <- regex-sof 
                       atoms <- (zero-many (choice regex-zero-one
                                                   regex-zero-many
                                                   regex-one-many
                                                   regex-range 
                                                   regex-atom
                                                   ))
                       eof <- regex-eof 
                       EOF 
                       (return `(,regex-parser* ,@(if (char=? sof #\^) 
                                                      `(,SOF)
                                                      '())
                                                ,@atoms
                                                ,@(if (char=? eof #\$)
                                                      `(,EOF)
                                                      '())))))

;; regex-parser 
;; convert the regexp into an useable parser, which including determining 
;; whether to allow for 
(define (regex-parser parsers) 
  (let ((regexp (sequence parsers)))
    (if (eq? (car parsers) SOF)
        regexp 
        (seq v <- (choice regexp 
                          (seq any-char (regex-parser parsers)))
             (return v)))))

;; regex-parser* 
;; the variable arg form of regex-parser
(define (regex-parser* parser . parsers)
  (regex-parser (cons parser parsers)))

;; make-regex-exp 
;; wrapper over regex... 
(define (make-regex-exp in)
  (define (helper exp)
    (cond ((list? exp) (apply (car exp) (map helper (cdr exp))))
          (else exp)))
  ;; (trace helper)
  (let-values (((exp in)
                (regex-exp (make-input in))))
    (if (failed? exp) 
        (error 'make-regex-exp "the regular expression is invalid")
        (lambda (in) 
          ((helper exp) (make-input in))))))

(provide regex-parser 
         make-regex-exp
         )
