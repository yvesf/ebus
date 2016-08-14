#lang racket/base
(require rackunit
         rackunit/text-ui
         (prefix-in layer2- "../ebus/layer2.rkt"))

(define layer2-test
  (test-suite
   "Tests for Ebus Parser"
   (test-case
    "Test sample Master-Master Paket"
    (let
        ([paket (layer2-read-ebus 
		 (open-input-bytes 
		  (bytes
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
		   000 ; p5
		   002 ; p6
		   000 ; p7
		   010 ; p8
		   128 ; CRC
		   000 ; ACK
		   170 ; SYN
		   170)))])
      (check-eq? (layer2-ebus-paket-source paket) 003)
      (check-eq? (layer2-ebus-paket-destination paket) 241)
      (check-eq? (layer2-ebus-paket-primaryCommand paket) 008)
      (check-eq? (layer2-ebus-paket-secondaryCommand paket) 000)
      (check-eq? (layer2-ebus-paket-payloadLength paket) 008)
      (check-eq? (layer2-ebus-paket-payloadLength paket)
                 (length (layer2-ebus-paket-payload paket)))
      ))
   (test-case 
    "test invalid paket"
    (let
        ([paket (layer2-read-ebus 
                 (open-input-bytes 
                  (bytes
		   170  ;SYN
		   170  ;SYN
		   016  ;SRC
		   003  ;DEST
		   008  ;PRIM => sollwertuebertragungRegler
		   000  ;SEC  => sollwertuebertragungRegler
		   008  ;PAY
		   051  ;P1
		   042  ;P2
		   000  ;P3
		   009  ;P4
		   128  ;P5
		   019  ;P6
		   000  ;P7 | ACK
		   045  ;P8 | ???                                
		   170  ;SYN
		   170  ;SYN
		   )))])
      (check-eq? paket eof)
      ))
   ))
      

(exit (run-tests layer2-test))
