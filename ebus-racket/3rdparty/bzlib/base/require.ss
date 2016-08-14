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
;; require.ss - require-like syntaxes 
;; yc 10/19/2009 - first version 
(require (for-syntax scheme/base "syntax.ss")
         )

(define-syntax (provide/strip-prefix stx)
  (syntax-case stx () 
    ((~ prefix out ...) 
     (with-syntax (((in ...)
                    (syntax-map (lambda (s)
                                  (syntax-identifier-append #'prefix s))
                                #'(out ...))))
       #'(provide (rename-out (in out) ...))))))

(provide provide/strip-prefix)

