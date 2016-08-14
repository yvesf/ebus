#lang scheme/base
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASE.plt 
;; 
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; args.ss - utility for helping processing syntax-based arguments (does not belong here)
;; yc 9/21/2009 - first version 
;; yc 9/25/2009 - move from port.plt to base.plt 
(require (for-syntax scheme/base) 
         scheme/match)

;; convert an argument (and an optional argument) into an identifier
;; p => p 
;; (p v ...) => p 
(define (arg->identifier stx)
  (syntax-case stx ()
    (p
     (symbol? (syntax->datum #'p))
     #'p)
    ;; an optional arg.
    ((p . _)
     #'p)))

;; (a (b v1) #:c (c v2)) => (a b c) 
(define (args->identifiers stx)
  (syntax-case stx () 
    (()
     #'())
    ((p . rest)
     (keyword? (syntax->datum #'p))
     (args->identifiers #'rest))
    ((p . rest)
     #`(#,(arg->identifier #'p) . #,(args->identifiers #'rest)))))

(define (args->kw+identifiers stx)
  (syntax-case stx () 
    (()
     #'())
    ((p . rest)
     (keyword? (syntax->datum #'p))
     #`(p . #,(args->identifiers #'rest)))
    ((p . rest)
     #`(#,(arg->identifier #'p) . #,(args->identifiers #'rest)))))

(define (args->kw-identifiers stx)
  (syntax-case stx () 
    (()
     #'())
    ((p . rest) 
     (keyword? (syntax->datum #'p))
     #`(p . #,(args->identifiers #'rest)))
    ((p . rest)
     (args->kw-identifiers #'rest))))
;; (trace args->kw-identifiers)

(define (args->kw-args stx)
  (syntax-case stx () 
    (() 
     #'())
    ((p . rest)
     (keyword? (syntax->datum #'p))
     #'(p . rest))
    ((p . rest)
     (args->kw-args #'rest))))

(define (args->non-kw-identifiers stx)
  (syntax-case stx () 
    (()
     #'())
    ((p . rest)
     (keyword? (syntax->datum #'p))
     #'())
    ((p . rest)
     #`(#,(arg->identifier #'p) . #,(args->non-kw-identifiers #'rest)))))

(define (args->non-kw-args stx)
  (syntax-case stx () 
    (()
     #'())
    ((p . rest)
     (keyword? (syntax->datum #'p))
     #'())
    ((p . rest)
     #`(p . #,(args->non-kw-args #'rest)))))

(provide arg->identifier 
         args->identifiers 
         args->kw+identifiers
         args->kw-identifiers 
         args->non-kw-identifiers
         args->kw-args 
         args->non-kw-args
         )

;;; typed args... 
;;; a typed args look like an optional argument, except that 
;;; it has the following: 
;;; (id type?) (id type? default) 
(define (typed-arg? stx)
  (match (syntax->datum stx)
    ((list (? symbol? _) _) #t)
    ((list (? symbol? _) _ _) #t)
    (else #f)))

(define (typed-arg->arg stx)
  (syntax-case stx () 
    ((p type)
     #'p)
    ((p type default)
     #'(p default))))

(define (typed-args->args stx)
  (syntax-case stx ()
    (()
     #'())
    ((p . rest)
     (keyword? (syntax->datum #'p))
     #`(p . #,(typed-args->args #'rest)))
    ((p . rest)
     #`(#,(typed-arg->arg #'p) . #,(typed-args->args #'rest)))))

(define (typed-arg->type stx)
  (syntax-case stx () 
    ((p type)
     #'type)
    ((p type default)
     #'type)))

(define (typed-args->types stx)
  (syntax-case stx () 
    (()
     #'())
    ((p . rest)
     (keyword? (syntax->datum #'p))
     (typed-args->types #'rest))
    ((p . rest)
     #`(#,(typed-arg->type #'p) . #,(typed-args->types #'rest)))))

(provide typed-args->args
         typed-args->types
         typed-arg->arg
         typed-arg->type
         )



