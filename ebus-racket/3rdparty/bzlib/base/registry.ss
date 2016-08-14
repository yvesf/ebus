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
;; registry.ss - generalized key/value access (including an extensible condition object)
;; yc 9/8/2009 - first version 
;; yc 7/7/2010 - add registry-clear! & modified registry definition. 
(require mzlib/pconvert-prop
         scheme/port
         scheme/string
         scheme/contract
         "base.ss"
         )
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; registry
;; a abstraction over key/value pairs 

(define-struct registry (get set del make (table #:mutable)))

(define (registry-set! reg key val)
  (set-registry-table! reg
                     ((registry-set reg) (registry-table reg) key val)))

(define (registry-del! reg key)
  (set-registry-table! reg
                     ((registry-del reg) (registry-table reg) key)))

(define (registry-ref reg key (default #f))
  ((registry-get reg) (registry-table reg) key default))
;; (trace registry-ref)

(define (registry-clear! reg) ;; clearing the registry... we need to fill it with a default value, of course. 
  ;; that means we need a way to get the default value... does that mean we will have to empty out the whole value... 
  ;; is there a way to do so without adding a new field? 
  ;; it is completely unclear... hmm... 
  ;; a hash's function is make-hash... 
  ;; an immutable-hash's function is make-immutable-hash-helper... 
  ;; an assoc's function
  (set-registry-table! reg ((registry-make reg))))
  

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-hash-registry
(define (make-hash-registry (hash (make-hash)))
  (define (set hash key val)
    (hash-set! hash key val)
    hash)
  (define (del hash key)
    (hash-remove! hash key)
    hash)
  (define (make (value (make-hash)))
    (cond ((hash? value) value) 
          ((list? value)
           (let ((h (make-hash))) 
             (for-each (lambda (kv)
                         (hash-set! h (car kv) (cdr kv)))
                       value)
             h))
          (else (error 'make-hash-unknown-input "~a" value))))
  (make-registry hash-ref set del make (make hash)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-immutable-hash-registry
(define (make-immutable-hash-registry (hash (make-immutable-hash '())))
  (define (make (value (make-immutable-hash '())))
    (cond ((and (immutable? value) (hash? value)) value)
          ((hash? value) (make-immutable-hash (hash-map value cons)))
          ((list? value) (make-immutable-hash value))
          (else (error 'make-immutable-hash-unknown-input "~a" value))))
  (make-registry hash-ref hash-set hash-remove make (make hash)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-assoc-registry (not thread safe if multiple addition & deletion)
;; let's also a list registry via assoc.
(define (assoc-ref lst key (default #f))
  (define (assoc/cdr key value (default #f))
    (let ((value (assoc key value)))
      (if (not value) default
          (cdr value))))
  (assoc/cdr key lst default))
;; (trace assoc-ref)
;; if we just want to remove the first guy with the key... how to do that? not with filter.

(define (assoc-del lst key)
  (define (helper k kv)
    (equal? k (car kv)))
  ;; (trace helper)
  (remove key lst helper))

(define (assoc-set lst key val)
  (let ((exists? #f))
    (let ((lst (map (lambda (kv)
                      (cons (car kv)
                            (cond ((equal? (car kv) key)
                                   (set! exists? #t)
                                   val)
                                  (else (cdr kv)))))
                    lst)))
      (if exists? lst
          (cons (cons key val) lst)))))

(define (make-list (lst '())) 
    (if (list? lst) 
        lst 
        (error 'make-assoc-list-unknown-input "~a" lst)))

(define (make-assoc-registry (lst '()))
  (make-registry assoc-ref assoc-set assoc-del make-list (make-list lst)))

;; what can be passed into ? it must be a list of lists.
(define (list->assoc-registry lst)
  (define (helper kvs)
    (cons (car kvs)
          (make-assoc-registry (cdr kvs))))
  ;; (trace helper)
  (make-assoc-registry (map helper lst)))

(define (assoc-registry->list reg)
  (map (lambda (kv)
         (cons (car kv)
               (registry-table (cdr kv))))
       (registry-table reg)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cond-registry (takes in a cond & result pair).
(define (cond-ref lst key (default #f))
  (let ((it (assf (lambda (cond)
                    (cond key)) lst)))
    (if (not it) default
        (cdr it))))

(define (make-cond-registry (lst '()))
  (make-registry cond-ref assoc-set assoc-del make-list (make-list lst)))

(provide/contract
 (struct registry ((get (->* (any/c any/c)
                             (any/c)
                             any))
                   (set (-> any/c any/c any/c any))
                   (del (-> any/c any/c any))
                   (make (->* () 
                              (any/c) 
                              any/c))
                   (table any/c)))
 (registry-ref (->* (registry? any/c)
                    (any/c)
                    any))
 (registry-set! (-> registry? any/c any/c any))
 (registry-del! (-> registry? any/c any))
 (registry-clear! (-> registry? any))
 (make-hash-registry (->* ()
                          ((or/c list? hash?))
                          registry?))
 (make-immutable-hash-registry (->* ()
                                    ((or/c list? (and/c immutable? hash?)))
                                    registry?))
 (assoc-ref (->* (list? any/c)
                 (any/c)
                 any))
 (assoc-set (-> list? any/c any/c any))
 (assoc-del (-> list? any/c any))
 (make-assoc-registry (->* ()
                           (list?)
                           registry?))
 (list->assoc-registry (-> list? registry?))
 (assoc-registry->list (-> registry? list?))
 (make-cond-registry (->* ()
                          (list?)
                          registry?))
 )

;; let's see how something can be flushed...
(define (registry->out reg out)
  (write (registry-table reg) out))

(define (registry->string reg)
  (let ((out (open-output-bytes)))
    (registry->out reg out)
    (get-output-string out)))

(define (in->registry in)
  (let ((value (read in)))
    (cond ((list? value)
           (make-assoc-registry value))
          ((and (hash? value) (immutable? value))
           (make-immutable-hash-registry value))
          ((hash? value)
           (make-hash-registry value))
          ((eof-object? value)
           (make-assoc-registry))
          (else
           (error 'in->registry "unknown registry type ~a" value)))))

(define (string->registry string)
  (in->registry (open-input-string string)))

(provide/contract
 (registry->out (-> registry? output-port? any))
 (registry->string (-> registry? string?))
 (in->registry (-> input-port? registry?))
 (string->registry (-> string? registry?))
 )

