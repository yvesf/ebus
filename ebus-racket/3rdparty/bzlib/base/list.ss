
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
;; list.ss - basic functionalities that has to do with list processing.
;; yc 9/8/2009 - first version 
;; yc 9/25/2009 - moved group from bzlib/dbd-memcached/dht to here; exported scheme/list 
;; yc 2/10/2010 - move listof? from assert.ss (not sure why it was there) to list.ss 
(require "base.ss" scheme/list scheme/contract)

(define (assoc/cdr key alist (default #f))
  (if-it (assoc key alist)
         (cdr it)
         default))

(define (assoc/s key alist (default '()))
  (let ((it (filter (lambda (kv)
                      (equal? (car kv) key))
                    alist)))
    (if (null? it) default it)))

;; this function is a combo of member & assoc
;; it's useful when we have a malformed alist, where when the
;; pair has no value, the key is retained
;; (or when there is no key, the value is retained)
(define (assoc* key lst (default #f))
  (define (helper rest)
    (cond ((null? rest) default)
          ;; assoc behavior
          ((and (pair? (car rest))
                (equal? key (caar rest)))
           (car rest))
          ;; member behavior
          ((and (not (pair? (car rest)))
                (equal? key (car rest)))
           rest)
          (else
           (helper (cdr rest)))))
  ;; (trace helper)
  (helper lst))

(define (assoc*/cdr key lst (default #f))
  (if-it (assoc* key lst)
         (cdr it)
         default))


(define (group alist)
  ;; for each alist with the same key - group them together!!
  (foldl (lambda (kv interim)
           (if-it (assoc (car kv) interim) ;; the key already exists...
               (cons (cons (car it) (cons (cdr kv) (cdr it)))
                     (filter (lambda (kv)
                               (not (equal? it kv))) interim))
               (cons (list (car kv) (cdr kv)) interim)))
         '()
         alist))


(define (list->unique lst (equal? equal?))
  (reverse (foldl (lambda (item interim)
                    (if (memf (lambda (item1)
                                (equal? item item1))
                              interim)
                        interim
                        (cons item interim)))
                  '()
                  lst)))

(define (listof? type?)
  (lambda (args)
    (and (list? args)
         (andmap type? args))))


(provide/contract 
 (assoc/cdr (->* (any/c list?)
                 (any/c)
                 any))
 (assoc/s (->* (any/c list?)
               (any/c)
               any))
 (assoc* (->* (any/c list?)
              (any/c)
              any))
 (assoc*/cdr (->* (any/c list?)
                  (any/c)
                  any))
 (group (-> (or/c null? pair?) any))
 (list->unique (->* (pair?)
                    (procedure?)
                    any))
 (listof? (-> isa/c isa/c))
 )
(provide (all-from-out scheme/list))

