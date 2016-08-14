#lang scheme/base
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSEQ.PLT 
;; A Parser Combinator library.
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calc.ss - a simple arithmetic calculator  
;; yc 12/31/2009 - first version 
(require "../main.ss"
         )

;; determine the operator (currently there are no precedences)... 
(define OP (tokens op <- (char-in '(#\+ #\- #\* #\/))
                   (return (case op 
                             ((#\+) +)
                             ((#\-) -)
                             ((#\*) *)
                             ((#\/) /)))))

(define NUMBER (token real-number)) 

;; expr := term op term 
(define expr (tokens lhs <- term 
                     (let loop ((lhs lhs))
                       (choice (tokens opr <- OP 
                                       rhs <- term 
                                       (loop (list opr lhs rhs)))
                               (return lhs)))))
;; term := factor op factor 
(define term (tokens lhs <- factor 
                     (let loop ((lhs lhs))
                       (choice (tokens opr <- OP 
                                       rhs <- factor 
                                       (loop (list opr lhs rhs)))
                               (return lhs)))))

;; factor := number | ( exp ) 
(define factor (choice NUMBER (bracket #\( expr #\))))

(define (calc in) 
  (define (helper exp)
    (cond ((number? exp) exp)
          ((pair? exp) 
           (apply (car exp) 
                  (map helper (cdr exp))))))
  (helper ((make-reader expr) in))) 

(provide calc) 
