#lang racket/base
(require (only-in racket/bool false?)
         "../3rdparty/bzlib/parseq/main.ss")

(define-logger ebus2)

(define ebus-const-syn #xaa) ;; SYN
(define ebus-const-escape #xa9) ;; Escape-Sequence Start
(define ebus-const-ackok #x00) ;; ACK
(define ebus-const-broadcastaddr #xfe) ;; Broadcast Address

(struct ebus-body-broadcast (crc) #:transparent)

(struct ebus-body-mastermaster (crc) #:transparent)

(struct ebus-body-masterslave
  (crc payloadSlaveLength payloadSlave crcSlave)
  #:transparent)

(struct ebus-paket
  (source destination primaryCommand secondaryCommand payloadLength payload body)
  #:transparent)

;; single, maybe escaped, payload data byte 
(define ebus-payload
  (choice (seq escape-seq <- ebus-const-escape
               escape-code <- (byte-in (list 0 1))
               (return (cond
                         ((= escape-code 0) ebus-const-escape)
                         ((= escape-code 1) bytes ebus-const-syn))))
          any-byte
          ))

(define parse-ebus-broadcast
  (token (seq crc <- any-byte
              syn <- ebus-const-syn
              (return (ebus-body-broadcast crc)))))

(define parse-ebus-mastermaster
  (token (seq  crc <- any-byte
               ack <- ebus-const-ackok ;; ACK des Empfängers
               syn <- ebus-const-syn   ;; SYN des Senders
               (return (ebus-body-mastermaster crc)))))

(define parse-ebus-masterslave 
  (token (seq crc <- any-byte
              ack <- ebus-const-ackok ;; ACK des Empfängers
              payloadSlaveLength <- any-byte
              payloadSlave <- (repeat ebus-payload payloadSlaveLength payloadSlaveLength)
              crcSlave <- any-byte
              ackSlave <- ebus-const-ackok ;; ACK des Senders
              synSlave <- ebus-const-syn   ;; SYN des Senders
              (return (ebus-body-masterslave crc payloadSlaveLength payloadSlave crcSlave)))))

(define parse-ebus-paket
  (token (seq source <- any-byte
              destination <- any-byte
              primaryCommand <- any-byte
              secondaryCommand <- any-byte
              payloadLength <- any-byte
              payload <- (repeat ebus-payload payloadLength payloadLength)
              body <- (cond ((= destination ebus-const-broadcastaddr) parse-ebus-broadcast)
                            (else (choice parse-ebus-mastermaster 
                                          parse-ebus-masterslave)))
              (return (ebus-paket source 
                                  destination
                                  primaryCommand
                                  secondaryCommand
                                  payloadLength
                                  payload 
                                  body)))))

(define ebus-sync (tokens syncs <- (seq (repeat (string->bytes/latin-1 "\xaa")))
                          (return (length syncs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-ebus input-port)
  (define syn   ((make-reader ebus-sync #:sof? #f #:eof? #f)  input-port))
  (define paket   ((make-reader parse-ebus-paket #:sof? #f #:eof? #f) input-port))
  (cond ((not (false? syn)) (log-ebus2-debug "dropped ~a x SYN (~a)" syn ebus-const-syn)))
  (cond ((not (false? paket)) paket)
        ((eof-object? (peek-byte input-port)) eof)
        (else
         ;; skip one byte
         (let ([byte (read-byte input-port)])
           (log-ebus2-debug "drop ~s 0x~x" byte byte))
         (read-ebus input-port))))

(provide 
 ;; Read Layer Ebus-Paket `ebus-paket`
 read-ebus
 ;; Expose datastructures
 (struct-out ebus-paket)
 (struct-out ebus-body-broadcast)
 (struct-out ebus-body-mastermaster)
 (struct-out ebus-body-masterslave))
