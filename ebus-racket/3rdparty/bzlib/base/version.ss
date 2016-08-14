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
;; version.ss - version comparison utilities as well as version-based macros 
;; yc 1/18/2010 - first version 
(require (prefix-in v: version/utils)
         scheme/contract
         (for-syntax scheme/base
                     (prefix-in v: version/utils))
         mzlib/trace
         )

(define (version? v)
  (and (string? v)
       (integer? (v:version->integer v))))

(define (vcomp? comp? v v2 vs) 
  (apply comp? (map v:version->integer (list* v v2 vs))))

(define (version<? v v2 . vs)
  (vcomp? < v v2 vs)) 
;; (trace version<?)

(define (version<=? v v2 . vs) 
  (vcomp? <= v v2 vs)) 
;; (trace version<=?)

(define (version>=? v v2 . vs)
  (vcomp? >= v v2 vs)) 
;; (trace version>=?)

(define (version>? v v2 . vs)
  (vcomp? > v v2 vs))
;; (trace version>?)

(define (version=? v v2 . vs)
  (vcomp? = v v2 vs)) 
;; (trace version=?)

(define (version!=? v v2 . vs)
  (vcomp? (compose not =) v v2 vs))
;; (trace version!=?)

(define vcomp/c (->* (version? version?)
                     ()
                     #:rest (listof version?)
                     boolean?))

(provide/contract 
 (version? (-> any/c boolean?))
 (version<? vcomp/c) 
 (version<=? vcomp/c)
 (version>=? vcomp/c) 
 (version>? vcomp/c) 
 (version=? vcomp/c)
 (version!=? vcomp/c)
 )

