#lang scheme/base
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASE.plt 
;; 
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assert.ss - utility for verifying result of the values... 
;; yc 1/9/2010 - fixed let/assert! and let*/assert to allow for optional test function
;; yc 2/10/2010 - move listof? to list.ss 
(require (for-syntax scheme/base "args.ss")
         "base.ss"
         (only-in mzlib/etc identity)
         (prefix-in c: scheme/contract)
         )

(define-struct (exn:assert! exn) (test? exp expected actual))

(define (error/assert! test? exp expected actual (name 'assert!))
  (raise (make-exn:assert! (if (not expected)
                               (format "~a assert! (~a ~a); actual ~a" name test? exp actual)
                               (format "~a assert! (~a ~a ~a); actual ~a" name test? exp expected actual))
                           (current-continuation-marks) test? exp expected actual)))

;; assert! v test? v2
;; assert! v true? 
;; assert! v v2 (use equal for comparison) => we can get rid of this form... 
(define-syntax named-assert!
  (syntax-rules () 
    ((~ name exp test? expected)
     (let ((actual exp))
       (if (test? actual expected)
           actual
           (error/assert! 'test? 'exp 'expected actual 'name))))
    ((~ name exp test?)
     (let ((actual exp))
       (if (test? actual)
           actual
           (error/assert! 'test? 'exp #f actual 'name))))
    ((~ name exp)
     (named-assert! name exp identity))
    ))

(define-syntax assert!
  (syntax-rules () 
    ((~ args ...)
     (named-assert! assert! args ...))))


(define-syntax let/assert!
  (syntax-rules () 
    ((~ ((id test? arg) ...) exp exp2 ...)
     (let/assert! let/assert! ((id test? arg) ...) exp exp2 ...))
    ((~ name ((id test? arg) ...) exp exp2 ...)
     (let ((id arg) ...)
       (let ((id (named-assert! name id test?)) ...) exp exp2 ...)))
    ((~ ((id arg) ...) exp exp2 ...)
     (let/assert! let/assert! ((id identity arg) ...) exp exp2 ...))
    ((~ name ((id arg) ...) exp exp2 ...) 
     (let/assert! name ((id identity arg) ...) exp exp2 ...)) 
    ))

(define-syntax let*/assert! 
  (syntax-rules ()
    ((~ name () exp exp2 ...)
     (begin exp exp2 ...))
    ((~ ((id test? arg) ...) exp exp2 ...)
     (let*/assert! let*/assert! ((id test? arg) ...) exp exp2 ...))
    ((~ name ((id test? arg) rest ...) exp exp2 ...)
     (let/assert! name ((id test? arg))
                  (let*/assert! name (rest ...) exp exp2 ...)))
    ((~ ((id arg) ...) exp exp2 ...)
     (let*/assert! ((id identity arg) ...) exp exp2 ...))
    ((~ name ((id arg) ...) exp exp2 ...) 
     (let*/assert! name ((id identity arg) ...) exp exp2 ...))
    ))

(define-syntax (lambda/assert! stx)
  (syntax-case stx () 
    ((~ name (a1 ... rest-id rest-type) exp exp2 ...) 
     (and (symbol? (syntax->datum #'name)) 
          (symbol? (syntax->datum #'rest-id)))
     (with-syntax (((arg ...)
                    (typed-args->args #'(a1 ...)))
                   ((id ...)
                    (args->identifiers #'(a1 ...)))
                   ((type ...)
                    (typed-args->types #'(a1 ...)))
                   )
       #'(lambda (arg ... . rest-id) 
           (let/assert! name ((id type id) ...
                              (rest-id rest-type rest-id))
                        exp exp2 ...))))
    ((~ name (a1 ...) exp exp2 ...) 
     (symbol? (syntax->datum #'name)) 
     (with-syntax (((arg ...)
                    (typed-args->args #'(a1 ...)))
                   ((id ...)
                    (args->identifiers #'(a1 ...)))
                   ((type ...)
                    (typed-args->types #'(a1 ...)))
                   )
       #'(lambda (arg ...) ;; this is the general idea.. but this general idea doesn't fully work... 
           (let/assert! name ((id type id) ...)
                        exp exp2 ...))))
    ((~ (a1 ...) exp exp2 ...)
     #'(~ lambda/assert! (a1 ...) exp exp2 ...))
    ))
  
(define-syntax define/assert!
  (syntax-rules () 
    ((~ (name . args) exp exp2 ...)
     (define name 
       (lambda/assert! name args exp exp2 ...)))))

(provide define/assert!
         lambda/assert!
         let*/assert!
         let/assert!
         assert!
         named-assert!
         )

(c:provide/contract
 (struct exn:assert! ((message string?)
                      (continuation-marks continuation-mark-set?)
                      (test? c:any/c)
                      (exp c:any/c)
                      (expected c:any/c)
                      (actual c:any/c)))
 (error/assert! (c:->* (c:any/c c:any/c c:any/c c:any/c)
                     (symbol?)
                     c:any))
 )

#|
;; if I want to define a contract... with the following form it can become quite complicated!!! 

;; we can also guard the arguments @ regular lamda and also let statement... 
;; guarding the arguments...
(define/assert! (foo (a number?) (b number? 5) #:c (c number? 5))
    (+ a b c))

(define/assert! (foo2 (a number?) (b number? 10) . (rest (listof? number?)))
  (apply + a b rest))
(let/assert! ((a number? 3) (b number? 'abc)) 
             (+ a b))
;;|#