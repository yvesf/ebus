#lang scheme/base
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSEQ.PLT 
;; A Parser Combinator library.
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; token.ss - token-based parser combinators. 
;; yc 1/5/2010 - first version 
;; yc 1/31/2010 - add tokens/by to allow for custom tokenizer, fix token to consume trailing whitespaces as well... 
(require "primitive.ss"
         "combinator.ss"
         "basic.ss"
         "input.ss"
         mzlib/defmacro
         (for-syntax scheme/base
                     scheme/match 
                     ) 
         scheme/list
         )

;; token
;; tokenizing a particular value... 
(define (token parser (delim whitespaces)) 
  (seq delim 
       t <- parser 
       delim 
       (return t))) 

(define (token/pre parser (delim whitespaces))
  (seq delim t <- parser (return t)))

(define-macro (tokens/by tokenizer . exps)
  (define (body exps) 
    (match exps 
      ((list exp) (list exp)) 
      ((list-rest v '<- exp rest) 
       `(,v <- (,tokenizer ,exp) . ,(body rest)))
      ((list-rest exp rest) 
       `((,tokenizer ,exp) . ,(body rest)))))
  `(seq . ,(body exps)))

;; tokens 
;; generating a sequence of tokens... 
(define-macro (tokens . exps) 
  `(tokens/by token . ,exps))

;; token-ci 
;; the literal tokens for string & character are case-insensitive 
(define-macro (tokens-ci . exps)
  `(tokens/by (compose token literal-ci) . ,exps))

;; alternate 
;; alternate between 2 parsers - ideally used for parsing delimited input
;; you can choose whether you want to have the delimiter returned... 
(define (alternate parser1 parser2)
  (tokens v <- parser1 
          v2 <- (zero-many (seq v1 <- parser2 
                                v3 <- parser1 
                                (return (list v1 v3))))
          (return (flatten (cons v v2)))))

;; delimited 
;; same as alternate, except the delimiters are parsed out and not returned 
(define (delimited parser delim (tokenizer token)) 
  (tokens/by tokenizer 
             v <- parser 
             v2 <- (zero-many (tokens/by tokenizer
                                         v3 <- delim
                                         v4 <- parser
                                         (return v4)))
             (return (cons v v2))))

;; bracket 
;; parsing bracketed structures... 
(define (bracket open parser close) 
  (tokens open
          v <- parser 
          close 
          (return v))) 

;; bracket/delimited 
(define (bracket/delimited open parser delim close) 
  (tokens open ;; even the parser is optional...  
          v <- (zero-one (delimited parser delim) '()) 
          close 
          (return v)))

(provide token 
         token/pre 
         tokens/by
         tokens
         tokens-ci
         alternate
         delimited 
         bracket
         bracket/delimited
         )
