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
;; bytes.ss - provides utility functions that works with bytes 
;; yc 10/19/2009 - first version 
;; yc 10/23/2009 - add read-bytes-avail that'll return the currently available bytes 
;; yc 10/24/2009 - add read-byte-list & read-byte-list/timeout 
;; yc 1/18/2010 - fix the issue that call-with-output-bytes was not available until v > 4.2
;; yc 2/5/2010 - added string-char-ratios for accurately determien the ratio of ascii/latin-1/unicode chars 
(require scheme/port scheme/contract "version-case.ss" "base.ss")

;; call-with-output-bytes is not available until 4.1 
(define *call-with-output-bytes 
  (+:version>= "4.2" 
               call-with-output-bytes
               (lambda (proc)
                 (let ((out (open-output-bytes))) 
                   (dynamic-wind void 
                                 (lambda () 
                                   (proc out)) 
                                 (lambda () 
                                   (get-output-bytes out)))))))

(define (port->bytes/charset in charset-in charset-out)
  (*call-with-output-bytes 
   (lambda (out)
     (convert-stream charset-in in charset-out out))))

(define (bytes->bytes/charset bytes charset-in charset-out)
  (port->bytes/charset (open-input-bytes bytes) charset-in charset-out))

(define (bytes/charset->bytes/utf-8 bytes charset)
  (bytes->bytes/charset bytes charset "utf-8")) 

(define (bytes/utf-8->bytes/charset bytes charset)
  (bytes->bytes/charset bytes "utf-8" charset))

;; there are more to handle (specifically charsets).
(define (bytes/charset->string bytes charset)
  (bytes->string/utf-8 (bytes/charset->bytes/utf-8 bytes charset)))

(define (string->bytes/charset string charset)
  (bytes/utf-8->bytes/charset (string->bytes/utf-8 string) charset))

(define (char-latin-1? c)
  (< 0 (char->integer c) 256))

(define (char-ascii? c)
  (< 0 (char->integer c) 128))

(define (string-char-or? s test?)
  (define (helper len i)
    (if (= len i) #f
        (if (test? (string-ref s i)) #t
            (helper len (add1 i)))))
  (helper (string-length s) 0)) 

(define (string-char-and? s test?)
  (define (helper len i)
    (if (= len i) #t
        (if (not (test? (string-ref s i))) #f
            (helper len (add1 i)))))
  (helper (string-length s) 0))

(define (char-type c)
  (let ((i (char->integer c))) 
    (cond ((< i 128) 'ascii)
          ((< i 256) 'latin-1)
          (else 'unicode))))

(define (string-char-ratios s) 
  (define (helper ascii latin-1 unicode i len)
    (if (= i len)
        (values (/ ascii len)
                (/ latin-1 len)
                (/ unicode len))
        (case (char-type (string-ref s i))
          ((ascii) (helper (add1 ascii) latin-1 unicode (add1 i) len))
          ((latin-1) (helper ascii (add1 latin-1) unicode (add1 i) len))
          (else (helper ascii latin-1 (add1 unicode) (add1 i) len)))))
  (if (= (string-length s) 0) 
      (values 1 0 0)
      (helper 0 0 0 0 (string-length s))))

(define (string-type s)
  (define (helper len i prev)
    (if (= len i) prev
        (let ((type (char-type (string-ref s i))))
          (case type 
            ((unicode) type)
            ((latin-1) 
             (helper len (add1 i) (case prev
                                    ((ascii) type)
                                    (else prev))))
            (else (helper len (add1 i) prev))))))
  (helper (string-length s) 0 'ascii))

(define (string-latin-1? s)
  (string-char-and? s char-latin-1?))

(define (string-ascii? s)
  (string-char-and? s char-ascii?))

(define (char->bytes c)
  (string->bytes/utf-8 (string c)))

(define (split-string-by-bytes-count str num)
  (define (maker chars)
    (list->string (reverse chars)))
  (define (helper str i chars blen acc)
    (if (= i (string-length str)) ;; we are done here!!!... 
        (reverse (if (null? chars) acc
                     (cons (maker chars) acc)))
        (let* ((c (string-ref str i))
               (count (char-utf-8-length c))) 
          (if (> (+ count blen) num) ;; we are done with this version....
              (if (= blen 0) ;; this means the character itself is greater than the count.  
                  (helper str (add1 i) '() 0 (cons (maker (cons c chars)) acc))
                  (helper str i '() 0 (cons (maker chars) acc)))
              (helper str (add1 i) (cons c chars) (+ count blen) acc)))))
  (helper str 0 '() 0 '()))

(define (read-bytes-avail num in)
  (define (helper bytes)
    (let ((len (read-bytes-avail!* bytes in 0 num)))
      (cond ((eof-object? len) bytes)
            ((number? len) (subbytes bytes 0 len))
            (else ;; this is a *special* value... I don't know what to do with it yet...                                                                        
             (len)))))
  (helper (make-bytes num 0)))

(define (read-byte-list num in)
  (define (helper bytes)
    (if (eof-object? bytes)
        bytes
        (bytes->list bytes)))
  (helper (read-bytes num in)))

(define (read-byte-list/timeout num in (timeout #f))
  (define (helper alarm acc count)
    (let ((evt (sync alarm in))) 
      (if (eq? alarm evt)
          (reverse acc)
          (let ((b (read-byte in))) 
            (cond ((eof-object? b) 
                   (if (null? acc) 
                       b
                       (reverse acc)))
                  ((= (add1 count) num)
                   (reverse (cons b acc)))
                  (else
                   (helper alarm (cons b acc) (add1 count))))))))
  (helper (alarm-evt (+ (current-inexact-milliseconds) (* 1000 (if (not timeout)
                                                                   +inf.0
                                                                   timeout)))) '() 0))

(define (read-bytes/timeout num in (timeout #f))
  (define (helper bytes)
    (if (eof-object? bytes) 
        bytes
        (list->bytes bytes))) 
  (helper (read-byte-list/timeout num in timeout))) 

(define (positive-number? n)
  (and (number? n) (> n 0))) 

(provide/contract
 (char-ascii? (typeof/c char?))
 (char-latin-1? (typeof/c char?)) 
 (string-char-or? (-> string? (-> char? any) any)) 
 (string-char-and? (-> string? (-> char? any) any)) 
 (string-latin-1? (typeof/c string?)) 
 (string-ascii? (typeof/c string?)) 
 (char-type (typeof/c char?)) 
 (string-char-ratios (-> string? (values number? number? number?)))
 (string-type (typeof/c string?))
 (split-string-by-bytes-count (-> string? exact-positive-integer? (listof string?)))
 (port->bytes/charset (-> input-port? string? string? any)) 
 (bytes->bytes/charset (-> bytes? string? string? bytes?))
 (bytes/charset->bytes/utf-8 (-> bytes? string? bytes?))
 (bytes/utf-8->bytes/charset (-> bytes? string? bytes?))
 (bytes/charset->string (-> bytes? string? string?))
 (string->bytes/charset (-> string? string? bytes?)) 
 (read-bytes-avail (-> exact-positive-integer? input-port? bytes?))
 (read-byte-list (-> exact-positive-integer? input-port? bytes?)) 
 (read-bytes/timeout (->* (exact-positive-integer? input-port?)
                          ((or/c #f positive-number?))
                          bytes?))
 (read-byte-list/timeout (->* (exact-positive-integer? input-port?)
                               ((or/c #f positive-number?)) 
                               any))
 )

