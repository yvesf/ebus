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
;; version-case.ss - version-based macros 
;; yc 1/18/2010 - first version 
(require (for-syntax scheme/base
                     "version.ss"
                     )
         "version.ss"
         )

(define-syntax (+:version stx)
  (syntax-case stx (between > >= < <= = != else)
    ((~) #'(void))
    ((~ (else exp)) #'exp)
    ((~ ((between min max) exp) rest ...)
     (version<=? (syntax->datum #'min) 
                 (version) 
                 (syntax->datum #'max))
     #'exp)
    ((~ ((between min max) exp) rest ...)
     #'(~ rest ...)) 
    ((~ ((> v) exp) rest ...)
     (version>? (version) (syntax->datum #'v))
     #'exp)
    ((~ ((> v) exp) rest ...)
     #'(~ rest ...)) 
    ((~ ((>= v) exp) rest ...)
     (version>=? (version) (syntax->datum #'v))
     #'exp)
    ((~ ((>= v) exp) rest ...)
     #'(~ rest ...)) 
    ((~ ((< v) exp) rest ...)
     (version<? (version) (syntax->datum #'v))
     #'exp)
    ((~ ((< v) exp) rest ...)
     #'(~ rest ...)) 
    ((~ ((<= v) exp) rest ...)
     (version<=? (version) (syntax->datum #'v))
     #'exp) 
    ((~ ((<= v) exp) rest ...)
     #'(~ rest ...)) 
    ((~ ((= v) exp) rest ...)
     (version=? (version) (syntax->datum #'v))
     #'exp) 
    ((~ ((= v) exp) rest ...)
     #'(~ rest ...)) 
    ((~ ((!= v) exp) rest ...)
     (version!=? (version) (syntax->datum #'v))
     #'exp)
    ((~ ((!= v) exp) rest ...)
     #'(~ rest ...)) 
    )) 

(define-syntax +:version-between
  (syntax-rules ()
    ((~ min max exp otherwise)
     (+:version ((between min max) exp) (else otherwise)))
    ))

(define-syntax define-version-if 
  (syntax-rules () 
    ((~ name comp)
     (define-syntax name 
       (syntax-rules () 
         ((~ v exp otherwise) 
          (+:version ((comp v) exp) (else otherwise))))))
    ))

(define-version-if +:version> >)

(define-version-if +:version>= >=)

(define-version-if +:version< <)

(define-version-if +:version<= <=)

(define-version-if +:version= =)

(define-version-if +:version!= !=)

(define-syntax require/v 
  (syntax-rules () 
    ((~ (test s1 ...) ...) 
     (+:version (test (require s1 ...)) ...))
    ))

(define-syntax provide/v
  (syntax-rules () 
    ((~ (test s1 ...) ...) 
     (+:version (test (provide s1 ...)) ...))
    ))

(provide +:version
         +:version-between
         +:version>
         +:version>=
         +:version<
         +:version<=
         +:version=
         +:version!=
         require/v 
         provide/v 
         )

