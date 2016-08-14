#lang scheme/base
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSEQ.PLT 
;; A Parser Combinator library.
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; combinator.ss - higher level combinator for parsers... 
;; yc 12/31/2009 - first version 
;; yc 1/5/2010 - moved delimited, bracket, and alternate to token.ss 
(require "depend.ss"
         mzlib/defmacro
         (for-syntax scheme/base
                     "depend.ss"
                     scheme/match
                     )
         "primitive.ss" 
         "input.ss" 
         ) 

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser COMBINATORS 

;; bind 
;; Parser a -> (a -> Parser b) -> Parser b 
;; this is the function version of the monad - use this when you want to 
;; create higher combinators dynamically... 
(define (bind parser v->parser) 
  (lambda (in)
    (let-values (((v in)
                  (parser in)))
      ((v->parser v) in))))

;; result 
;; allows the transformation of the result of the parser... 
(define (result parser transform)
  (bind parser 
        (lambda (v) 
          (if (succeeded? v) 
              (return (transform v))  
              fail))))

(define (result* parser transform)
  (bind parser 
        (lambda (v) 
          (if (and (succeeded? v) (list? v))
              (return (apply transform v))
              fail))))

;; seq 
;; the macro-based monad for stringing multiple parsers together... 
;; (seq parser) => parser 
;; (seq v <- parser exp ...) => (bind paser (lambda (v) (if v (seq exp ...) fail))
(define-macro (seq . exps) 
  (define *in (gensym 'in)) ;; represents the input 
  (define *v (gensym 'v)) ;; represents the value 
  (define literal 'literal) 
  ;; sequence body for creating a sequence combinator... 
  (define (body exps) 
    (match exps 
      ((list exp) 
       `((,literal ,exp) ,*in))
      ((list-rest var '<- exp rest)
        `(let-values (((,var ,*in)
                       ((,literal ,exp) ,*in)))
           (if (succeeded? ,var) 
               ,(body rest)
               (fail in))))
      ((list-rest exp rest) 
       (body `(,*v <- ,exp . ,rest)))
      ))
  `(lambda (in) 
     (let ((,*in in))
       ,(body exps))))

;; sequence 
;; a functional version of seq 
(define (sequence parsers) 
  (lambda (IN) 
    (define (helper parsers in acc) 
      (if (null? parsers) 
          ((return (reverse acc)) in)
          (let-values (((v in) 
                        ((car parsers) in))) 
            (if (succeeded? v) 
                (helper (cdr parsers) in (cons v acc))
                (fail IN)))))
    (helper (map literal parsers) IN '()))) 

;; sequence* 
(define (sequence* . parsers)
  (sequence parsers))

;; #|
;; choice 
;; (choice parser) => (bind parser (lambda (v) (if v (return v) fail)) 
;; (choice parser rest ...) => (bind parser (lambda (v) (if v (choice rest ...) fail))) 
(define-macro (choice . exps) 
  (define *in (gensym 'in)) ;; represents the input 
  (define *v (gensym 'v)) ;; represents the value 
  (define (body exps) 
    (match exps 
      ((list) 
       `(fail ,*in))
      ((list-rest exp rest) 
       `(let-values (((,*v ,*in)
                      ((literal ,exp) ,*in)))
          (if (succeeded? ,*v)
               ((return ,*v) ,*in)
               ,(body rest))))
      ))
  `(lambda (,*in)
     ,(body exps)))
;;|#

;; one-of 
;; a function version of choice 
(define (one-of parsers)
  (lambda (in) 
    (define (helper parsers) 
      (if (null? parsers)
          (fail in) 
          (let-values (((v in) 
                        ((car parsers) in))) 
            (if (succeeded? v) 
                ((return v) in)
                (helper (cdr parsers))))))
    (helper (map literal parsers))))

;; one-of* 
(define (one-of* . parsers)
  (one-of parsers)) 

;; all-of 
(define (all-of parsers) 
  (lambda (in) 
    (define (helper parsers v)
      (if (null? parsers)
          ((return v) in) 
          (let-values (((v IN) 
                        ((car parsers) in)))
            (if (succeeded? v) 
                (helper (cdr parsers) v)
                (fail in)))))
    (helper (map literal parsers) (make-failed 0))))

;; all-of* 
(define (all-of* . parsers) 
  (all-of parsers)) 

;; repeat 
;; returns when # of occurence falls within the min and max range 
;; default to [1,+inf] 
(define (repeat parser (min 1) (max +inf.0)) 
  (define (make parser) 
    (lambda (IN) 
      (define (helper prev-in acc count)
        (let-values (((v in)
                      (parser prev-in)))
          (if (succeeded? v) 
              (if (< count max) 
                  (helper in (cons v acc) (add1 count))
                  ((return (reverse acc)) prev-in))
              (if (< count min) 
                  (fail IN)
                  ((return (reverse acc)) in)))))
      (helper IN '() 0)))
  (make (literal parser))) 

;; zero-many 
;; returns the matched values if zero or more matches 
;; (this means that this parser will always match) 
(define (zero-many parser) 
  (repeat parser 0)) 

;; one-many 
;; matches if parser parses one or more times 
(define (one-many parser)
  (repeat parser)) 

;; zero-one 
;; returns if the parser matches zero or one times 
;; when the parser does not match, it defaults to fail, but you can pass in a 
;; default value so it does not fail. 
(define (zero-one parser default) 
  (lambda (in) 
    (let-values (((v in)
                  ((literal parser) in))) 
      ((return (if (succeeded? v) v default)) in))))

(provide bind
         result 
         result* 
         seq
         sequence
         sequence*
         choice
         one-of
         one-of* 
         all-of
         all-of* 
         repeat 
         zero-many
         one-many
         zero-one
         )
