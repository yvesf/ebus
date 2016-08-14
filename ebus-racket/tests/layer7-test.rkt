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
          (check-equal? (third p) 10))
        )))))

(exit (run-tests layer7-test))
