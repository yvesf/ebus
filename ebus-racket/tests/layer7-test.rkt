#lang racket/base
(require rackunit
         rackunit/text-ui
         racket/list
         (prefix-in layer2- "../ebus/layer2.rkt")
         (prefix-in layer7- "../ebus/layer7.rkt"))

(define layer7-test
  (test-suite
   "Tests for Ebus Layer 7 Parser"
   (test-case
    "Test sample Master-Master Paket"
    (parameterize ([layer7-definition (layer7-read-ebus-xml "../../ebus-xml/ebus.xml")])
      (let*
          ([l2paket (layer2-read-ebus (open-input-bytes (bytes
                                                         170 ; SYN
                                                         170
                                                         003 ; Source
                                                         241 ; Destination
                                                         008 ; primaryCommand
                                                         000 ; secondaryCommand
                                                         008 ; payloadLength
                                                         128 ; p1
                                                         040 ; p2
                                                         230 ; p3
                                                         002 ; p4
                                                         200 ; p5
                                                         002 ; p6
                                                         000 ; p7
                                                         010 ; p8
                                                         128 ; CRC
                                                         000 ; ACK
                                                         170 ; SYN
                                                         170)))]
           [fields (layer7-paket-parse l2paket)])
        (for ([field fields])
          (display field)
          (display "\n"))
        (check-true (= 5 (length fields)) "Anzahl der gelesenen Felder")
        (let ([p (first fields)])
          (check-equal? (first p) "feuerungsautomat1.sollwertuebertragungRegler.TK_soll")
          (check-equal? (second p) 'data2b)
          (check-equal? (third p) 40.5))
        (let ([p (second fields)])
          (check-equal? (first p) "feuerungsautomat1.sollwertuebertragungRegler.TA_ist")
          (check-equal? (second p) 'data2b)
          (check-equal? (third p) 2.8984375))
        (let ([p (third fields)])
          (check-equal? (first p) "feuerungsautomat1.sollwertuebertragungRegler.L_zwang")
          (check-equal? (second p) 'data1b)
          (check-equal? (third p) -56))
        (let ([p (fourth fields)])
          (check-equal? (first p) "feuerungsautomat1.sollwertuebertragungRegler.Status")
          (check-equal? (second p) 'bit)
          (check-equal? (third p) 0))
        (let ([p (fifth fields)])
          (check-equal? (first p) "feuerungsautomat1.sollwertuebertragungRegler.TB_soll")
          (check-equal? (second p) 'data2b)
          (check-equal? (third p) 10.0))
        )))
   (test-case
    "Test data2b: negative Werte"
    (parameterize ([layer7-definition (layer7-read-ebus-xml "../../ebus-xml/ebus.xml")])
      (define (ta-ist low high)
        (third (second (layer7-paket-parse
                        (layer2-read-ebus
                         (open-input-bytes
                          (bytes #xaa 3 241 8 0 8 128 40 low high 0 2 0 10 128 0 #xaa #xaa)))))))
      (check-equal? (ta-ist #x00 #xff) -1.0)
      (check-equal? (ta-ist #xff #xff) (/ -1 256.0))
      (check-equal? (ta-ist #x80 #xff) -0.5)
      (check-equal? (ta-ist #x00 #x9c) -100.0)
      (check-equal? (ta-ist #xe6 #x02) 2.8984375)))
   (test-case
    "Test Master-Slave Paket 50h 14h, Ist aus der Antwort des Slaves"
    (parameterize ([layer7-definition (layer7-read-ebus-xml "../../ebus-xml/ebus.xml")])
      (let* ([l2paket (layer2-read-ebus
                       (open-input-bytes
                        (bytes #xaa
                               #x70 #x51 #x50 #x14 #x07 ; Source, Destination, Command, Length
                               #x03 #x00 #x26 #x9a #x1f #x00 #x64 ; Payload Master
                               #x88 #x00 ; CRC, ACK
                               #x09 ; Length Slave
                               #x00 #xa1 #x80 #x25 #x00 #xd8 #x06 #x96 #x00 ; Payload Slave
                               #xe0 #x00 #xaa #xaa)))] ; CRC, ACK, SYN
             [fields (layer7-paket-parse l2paket)])
        (check-equal? fields
                      (list (list "heizkreisregler2.vorlauftemperatur.vorlauftemperaturIst" 'data2b 37.5)
                            (list "heizkreisregler2.vorlauftemperatur.vorlauftemperaturSoll" 'data2b 38.0))))))
   (test-case
    "Test Paket 50h 14h ohne Antwort: nur das Soll"
    (parameterize ([layer7-definition (layer7-read-ebus-xml "../../ebus-xml/ebus.xml")])
      (let* ([l2paket (layer2-read-ebus
                       (open-input-bytes
                        (bytes #xaa
                               #x70 #x51 #x50 #x14 #x07
                               #x03 #x00 #x26 #x9a #x1f #x00 #x64
                               #x88 #x00 #xaa #xaa)))]
             [fields (layer7-paket-parse l2paket)])
        (check-equal? fields
                      (list (list "heizkreisregler2.vorlauftemperatur.vorlauftemperaturSoll" 'data2b 38.0))))))))

(exit (run-tests layer7-test))
