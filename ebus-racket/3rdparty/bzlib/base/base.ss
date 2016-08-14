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
;; base.ss - basic functionalities that do not belong anywhere else.
;; yc 9/8/2009 - first version 
;; yc 9/25/2009 - moved assert! & let/assert! to assert.ss 
;; yc 1/12/2010 - add let*/if 
;; yc 2/5/2010 - add define-case-test & case/equal? & case/string-ci=?
;; yc 2/13/2010 - add isa/c 
(require (for-syntax scheme/base)
         scheme/list
         scheme/port
         mzlib/etc
         mzlib/trace
         scheme/contract
         scheme/function 
         )

(define-syntax (trace-lambda stx)
  (syntax-case stx ()
    ((~ args exp exp2 ...)
     #'(letrec ((func
                 (lambda args exp exp2 ...)))
         (trace func)
         func))))

(define-syntax (if-it stx)
  (syntax-case stx ()
    [(src-if-it test then else)
     (syntax-case (datum->syntax (syntax src-if-it) 'it) ()
       [it (syntax (let ([it test]) (if it then else)))])]))

(define-syntax (when-it stx)
  (syntax-case stx ()
    ((~ test? exp exp2 ...)
     (with-syntax ((it (datum->syntax #'~ 'it)))
       #'(let ((it test?)) (when it exp exp2 ...))))))

(define-syntax (cond-it stx)
  (syntax-case stx (else)
    ((cond-it (else exp exp2 ...))
     #'(begin exp exp2 ...))
    ((cond-it (test? exp exp2 ...))
     (with-syntax ((it (datum->syntax #'cond-it 'it)))
       #'(let ((it test?)) (when it exp exp2 ...))))
    ((cond-it (test? exp exp2 ...) cond cond2 ...)
     (with-syntax ((it (datum->syntax #'cond-it 'it)))
       #'(let ((it test?))
           (if it (begin exp exp2 ...)
               (cond-it cond cond2 ...)))))))

(define-syntax while
  (syntax-rules ()
    ((while test exp exp2 ...)
     (let loop ()
       (when test
         exp exp2 ...
         (loop))))
    ))

(define-syntax let*/if 
  (syntax-rules () 
    ((~ ((arg val)) exp exp2 ...) 
     (let ((arg val)) 
       (if (not arg) 
           #f
           (begin exp exp2 ...))))
    ((~ ((arg val) (arg-rest val-rest) ...) exp exp2 ...) 
     (let ((arg val)) 
       (if (not arg) 
           #f 
           (let*/if ((arg-rest val-rest) ...) exp exp2 ...))))))

(define-syntax case/pred?
  (syntax-rules (else) 
    ((~ pred? (else exp exp2 ...))
     (begin exp exp2 ...))
    ((~ pred? ((d d2 ...) exp exp2 ...))
     (when (ormap pred? (list d d2 ...))
       exp exp2 ...))
    ((~ pred? ((d d2 ...) exp exp2 ...) rest ...)
     (if (ormap pred? (list d d2 ...))
         (begin exp exp2 ...)
         (case/pred? pred? rest ...)))))

(define-syntax define-case/test?
  (syntax-rules () 
    ((~ name test?)
     (define-syntax name 
       (syntax-rules ()
         ((~ v clause clause2 (... ...))
          (case/pred? (curry test? v) clause clause2 (... ...)))))
    )))


(define-case/test? case/equal? equal?)
(define-case/test? case/string-ci=? string-ci=?)

;;|#

;; (trace load-proc)
;; a generic version of apply & keyword-apply that requires
;; no sorting of the parameter args... 
(define (apply* proc . args)
  (define (filter-kws args (acc '()))
    (cond ((null? args) (reverse acc))
          ((keyword? (car args))
           (filter-kws (cdr args) (cons (car args) acc)))
          (else
           (filter-kws (cdr args) acc))))
  (define (filter-kw-vals args (acc '()))
    (cond ((null? args) (reverse acc))
          ((keyword? (car args))
           (if (null? (cdr args)) ;; this is wrong!!!
               (error 'kw-apply "keyword ~a not followed by a value" (car args))
               (filter-kw-vals (cddr args) (cons (cadr args) acc))))
          (else
            (filter-kw-vals (cdr args) acc))))
   (define (filter-non-kw-vals args (acc '()))
     (cond ((null? args) (reverse acc))
           ((keyword? (car args))
            (if (null? (cdr args))
                (error 'kw-apply "keyword ~a not followed by a value" (car args))
                (filter-non-kw-vals (cddr args) acc)))
           (else
            (filter-non-kw-vals (cdr args) (cons (car args) acc)))))
   (define (sorted-kw+args args)
     (let ((kw+args (sort (map (lambda (kw vals)
                                 (cons kw vals))
                               (filter-kws args)
                               (filter-kw-vals args))
                          (lambda (kv kv1)
                            (keyword<? (car kv) (car kv1))))))
       (values (map car kw+args) (map cdr kw+args))))
   (define (normalize-args args)
     (cond ((list? (last args))
            (apply list* args))
           (else (error 'apply* "Expect last arg as a list, given ~a" (last args)))))
   (let ((args (normalize-args args)))
     (let-values (((kws vals)
                   (sorted-kw+args args)))
       (keyword-apply proc kws vals
                      (filter-non-kw-vals args)))))



(define (value-or v (default #f))
  (if (not v) default v))

(define (null-or v (default #f))
  (if (null? v) default v))

(define (thunk? p)
  (and (procedure? p)
       (let ((a (procedure-arity p)))
         (cond ((arity-at-least? a)
                (= (arity-at-least-value a) 0))
               ((number? a) (= a 0))
               ((list? a) (member 0 a))))))

;; isa/c 
;; this is useful but I did not include it until a bit too late... hmm... 
(define isa/c (-> any/c any))

(define (typeof/c contract)
  (-> contract any))

(provide (all-from-out mzlib/etc
                       scheme/function
                       )
         trace-lambda
         if-it
         when-it
         cond-it
         while
         let*/if
         case/pred?
         define-case/test?
         case/equal?
         case/string-ci=?
         isa/c
         typeof/c
         )


(provide/contract
 (apply* (->* (procedure?)
              ()
              #:rest (listof any/c)
              any))
 (value-or (->* (any/c)
                (any/c)
                any))
 (null-or (->* (any/c)
               (any/c)
               any))
 (thunk? (-> any/c boolean?))
 )
