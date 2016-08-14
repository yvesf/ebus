#lang racket/base
;
; XML-Expression Path Lookup
;

(require racket/contract
         racket/string
         racket/match
         racket/dict
         racket/list
         xml)

(provide xexpr-path-first
         xexpr-path-list
         xexpr-path-text
         xexpr-path/c)


(define xexpr-path/c
  (listof (or/c symbol?
                (list/c symbol? string?)
                (list/c symbol?))))


(define (children element)
  (match element
    ((list tag (list (list name value) ...) children ...)
     children)

    ((list tag children ...)
     children)

    (else
     null)))


(define (attr-value?? name value)
  (lambda (v)
    (equal? (dict-ref (attributes v) name #f) value)))


(define (tag-name?? name)
  (lambda (v)
    (or (eq? name '*)
        (and (pair? v)
             (eq? (car v) name)))))


(define (attributes element)
  (match element
    ((list tag (list (list name value) ...) children ...)
     (for/list ((n (in-list name))
                (v (in-list value)))
       (cons n v)))

    (else
     null)))


(define (path-item-procedure item)
  (match item
    ((list attr-name attr-value)
     (lambda (tags)
       (list (filter (attr-value?? attr-name attr-value) tags))))

    ((list attr-name)
     (lambda (tags)
       (list
         (filter values
                 (for/list ((tag (in-list tags)))
                   (dict-ref (attributes tag) attr-name #f))))))

    (tag-name
     (lambda (tags)
       (for/list ((tag (in-list tags)))
         (filter (tag-name?? tag-name) (children tag)))))))


(define/contract (xexpr-path-list path xexpr)
                 (-> xexpr-path/c xexpr/c (listof (or/c xexpr/c string?)))
  (let ((pipeline (append* (for/list ((item (in-list path)))
                             (list (path-item-procedure item) append*)))))
    ((apply compose (reverse pipeline)) (list xexpr))))


(define/contract (xexpr-path-first path xexpr)
                 (-> xexpr-path/c xexpr/c (or/c xexpr/c string? #f))
  (let ((results (xexpr-path-list path xexpr)))
    (and (not (null? results))
         (first results))))


(define/contract (xexpr-path-text path xexpr)
                 (-> xexpr-path/c xexpr/c (or/c #f string?))
  (let ((results (xexpr-path-list path xexpr)))
    (string-append* (map xexpr->string results))))


; vim:set ts=2 sw=2 et:
