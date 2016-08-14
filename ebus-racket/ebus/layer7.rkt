#lang racket/base
(require 
  (only-in racket/bool false?)
  (only-in racket/function curry)
  (only-in xml collapse-whitespace
           xexpr-drop-empty-attributes
           xml->xexpr
           document-element
           read-xml)
  (only-in "../3rdparty/xexpr-path/main.rkt"
           xexpr-path-first
           xexpr-path-list)
  (prefix-in layer2- "layer2.rkt"))

(define-logger ebus7)

(define (ersatzwert) 'ersatzwert)
(define (ersatzwert? v) (eq? (ersatzwert) v))

(define (read-ebus-xml path)
  (let* ([in (open-input-file path)]
         [xexpr (parameterize ([collapse-whitespace #t]
                               [xexpr-drop-empty-attributes #t])
                  (xml->xexpr (document-element (read-xml in))))])
    (close-input-port in)
    xexpr))

(define definition (make-parameter null))

(define (paket ebus-paket)
  (define primaryCommand (number->string (layer2-ebus-paket-primaryCommand ebus-paket)))
  (define secondaryCommand (number->string (layer2-ebus-paket-secondaryCommand ebus-paket)))
  (log-ebus7-debug "Lookup paket primaryCommand=~a secondaryCommand=~a" primaryCommand secondaryCommand)
  (xexpr-path-first (list 'packets 'packet (list 'primary primaryCommand)
                          (list 'secondary secondaryCommand)) (definition)))

(define (paket-name xexpr)
  (xexpr-path-first '((name)) xexpr))

(define (paket-fields paket-definition)
  (filter 
   (lambda (i) (and (pair? i) (member (car i) '(bit bcd data1b data1c byte data2b data2c word byteEnum))))
   (xexpr-path-list '(fields *) paket-definition)))

;; returns the full device-definition
(define (device address)
  (xexpr-path-first (list 'devices 'device (list 'address (number->string address))) (definition)))

;; returns device-name in a list or empty-list
(define (device-name address)
  (xexpr-path-first (list 'devices 'device (list 'address (number->string address)) '(name)) (definition)))

(define (paket-parse ebus-paket)
  (define paket-definition (paket ebus-paket))
  (define source-device-name (device-name (layer2-ebus-paket-source ebus-paket)))
  (cond ((and (not (false? paket-definition)) (not (false? source-device-name)))
         (define paket-id (string-append source-device-name "." (paket-name paket-definition)))
         (define decoders (map (lambda (field) (create-decoder paket-id field)) (paket-fields paket-definition)))
         (define payload (layer2-ebus-paket-payload ebus-paket))
         (for/list ([decoder decoders])
           (decoder payload)))
        (else (void (log-ebus7-info "Unknown Paket from source ~s(~s): ~s" 
                                    (layer2-ebus-paket-source ebus-paket) 
                                    source-device-name 
                                    ebus-paket)))))


(define (create-decoder paket-id field)
  (define type (car field))
  (define name (string-append paket-id "." (xexpr-path-first '((name)) field)))
  (define offset (string->number (xexpr-path-first '((offset)) field)))
  (define decoder (hash-ref decoder-table type #f))
  (cond ((false? decoder) (void (log-ebus7-warning "No decoder for type ~s" type)))
        (else (curry (car decoder) name field offset))))

(define decoder-table
  (make-hash (list
              (list 'bit (lambda (name field offset payload)
                           (list name 'bit (field-decoder-bit (list-ref payload offset)))))
              (list 'byte (lambda (name field offset payload)
                            (list name 'byte (field-decoder-byte (list-ref payload offset)))))
              (list 'bcd (lambda (name field offset payload)
                           (list name 'bcd (field-decoder-bcd (list-ref payload offset)))))
              (list 'data1b (lambda (name field offset payload)
                              (list name 'data1b (field-decoder-data1b (list-ref payload offset)))))
              (list 'data1c (lambda (name field offset payload)
                              (list name 'data1c (field-decoder-data1c (list-ref payload offset)))))
              (list 'data2b (lambda (name field offset payload)
                              (list name 'data2b (field-decoder-data2b 
                                                  (list-ref payload offset)
                                                  (list-ref payload (+ offset 1))))))
              (list 'data2c (lambda (name field offset payload)
                              (list name 'data2c 
                                    (field-decoder-data2c (list-ref payload offset) (list-ref payload (+ offset 1))))))
              (list 'word (lambda (name field offset payload)
                            (list name 'word (field-decoder-word (list-ref payload offset) (list-ref payload (+ offset 1))))))
              (list 'byteEnum (lambda (name field offset payload)
                                (list name 'byteEnum (field-decoder-byteEnum (list-ref payload offset) field))))
              )))

;; type bit
(define (field-decoder-bit value)
  (cond ((= value 1) 1)
        (else 0)))

;; type byte
(define (field-decoder-byte value)
  (cond ((= value #xff) (ersatzwert))
        (else value)))

;; type data1b
(define (field-decoder-data1b value)
  (if (= value #x80)
      (ersatzwert)
      (cond ((= 1 (arithmetic-shift value -7))
             (* -1 (+ 1 (bitwise-xor #xff value))))
            (else value))))

;; type data1c
(define (field-decoder-data1c value)
  (if (= value #xff)
      (ersatzwert)
      (/ value 2.0)))

;; type data2b
(define (field-decoder-data2b lowByte highByte)
  (if (and (= highByte 128)  (= lowByte 0))
      (ersatzwert)
      (if (= (bitwise-and highByte 128) 128)
          (* -1 
             (+ (+ 256 (bitwise-not highByte))
                (/ (+ 256 (bitwise-not (+ lowByte 1))) 256.0)))
          (+ highByte (/ lowByte 256.0)))))

;; type data2c
;;    Beispiel für die Berechnung:
;;    if ((x & 8000h) == 8000h) // y negativ
;;    y = - [dez(High_Byte(!x)) 16 + dez(High_Nibble (Low_Byte (!x)))
;;    + (dez(Low_Nibble (Low_Byte (!x))) +1 ) / 16]
;;    else                       // y positiv
;;    y = dez(High_Byte(x)) 16 + dez(High_ Nibble (Low Byte (x)))
;;    + dez(Low_ Nibble (Low Byte (x))) / 16
(define (field-decoder-data2c lowByte highByte)
  (define (lowNibble v)
    (bitwise-and v #x0f))
  (define (highNibble v)
    (arithmetic-shift v -4))
  (define (u-not v)
    (+ 256 (bitwise-not v)))
  
  (if (and (= highByte 128)  (= lowByte 0))
      (ersatzwert)
      (if (= (bitwise-and highByte 128) 128)
          (* -1
             (+ (arithmetic-shift (u-not highByte) 4)
                (highNibble (u-not lowByte))
                (/ (+ (lowNibble (u-not lowByte)) 1)
                   16.0)))
          (+ (arithmetic-shift highByte 4)
             (highNibble lowByte)
             (/
              (lowNibble lowByte)
              16)))))

;; type byteEnum
(define (field-decoder-byteEnum value field-definition)
  (define (pred l)
    (= value (list-ref l 0)))
  (define all-options (for/list ([option (xexpr-path-list '(option) field-definition)])
                        (list (string->number (xexpr-path-first '((value)) option)) ;; '(value name)
                              (xexpr-path-first '((name)) option))))
  (define options (filter pred all-options))
  (cond ((= (length options) 1)
         (list-ref (car options) 1))
        (else (format "<undefined:~a>" value))))

;; type word
(define (field-decoder-word lowByte highByte)
  (define value
    (+ lowByte (arithmetic-shift highByte 8)))
  (if (= value #xffff)
      (ersatzwert)
      value))

;; type bcd
(define (field-decoder-bcd value)
  (cond ((= value #xff) (ersatzwert))
        (else (+ (bitwise-and value #x0f)
                 (* (arithmetic-shift value -4) 10)))))

;; read one ebus-paket or eof from input-port
;; or return #<eof>
(define (read-ebus input-port)
  (define paket (layer2-read-ebus input-port))
  (cond ((layer2-ebus-paket? paket)
         (paket-parse paket))
        (else paket)))

(provide ersatzwert
         ersatzwert?
         read-ebus-xml
         paket
         paket-parse
         paket-fields
         device
         definition
         ;; read ebus from port an return fields from next paket
         read-ebus)
