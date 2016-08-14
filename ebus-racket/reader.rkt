#!/usr/bin/env racket
#lang racket/base
(require racket/cmdline
         racket/stream
         racket/string
         data/queue
         net/url
         (prefix-in layer7- "ebus/layer7.rkt"))

(define-logger inserter)

(define insert? (make-parameter #f))
(define influx-url? (make-parameter null))
(define influx-queue (make-queue))
(define influx-queue-size? (make-parameter 0))
(define ebus-xml-path? (make-parameter "../ebus-xml/ebus.xml"))

;; Send fields to database server
(define (insert-influxdb sensor-name datatype value)
  (if (layer7-ersatzwert? value)
      (log-inserter-debug "Skipping Ersatzwert for ~a/~a" sensor-name datatype)
      ;; Some basic formatting rules. This must satisfy the influxdb "Write Protocol"
      ;; https://docs.influxdata.com/influxdb/v0.13/write_protocols/line/
      (let* ([raw-value (cond ((member datatype '(data1c data2b data2c)) (real->decimal-string (exact->inexact value)))
                              ((member datatype '(bit byte data1b word bcd)) (format "~s" value))
                              ((member datatype '(byteEnum)) (format "\"~s\"" value)))]
             [point (format "~a,type=~a value=~a" sensor-name (symbol->string datatype) raw-value)])
        (enqueue! influx-queue point)
        (log-inserter-debug (format "influxdb: ~a~n" point))
        (when (> (queue-length influx-queue) (influx-queue-size?))
          (let ([points (queue->list influx-queue)]) ;; empty the queue
            (log-inserter-info "Make bulk insert to ~a" (influx-url?))
            (for-each (lambda (e) (dequeue! influx-queue)) (queue->list influx-queue))
            (define input-port (post-impure-port (string->url (influx-url?))
                                                 (string->bytes/utf-8 (string-join points (format "~n")))))
            (log-inserter-info "Server Response: ~a~n" (read-line input-port))
            (log-inserter-info "Data: ~a~n" (string-join points "|"))
            (close-input-port input-port))))))

(define (handle-packet packet)
  (for ([field packet])
    (when (insert?)
      (with-handlers ([exn:fail? (lambda (exn)
                                   (log-inserter-error "Failed to insert ~a: ~a" field exn))]
                      [exn:fail:read? (lambda (exn)
                                        (log-inserter-error "TCP Read exception ~a" exn))]
                      [exn:fail:network? (lambda (exn)
                                           (log-inserter-error "TCP Exception ~a" exn))])
        (apply insert-influxdb field)))
    (when (not (insert?))
      (apply (lambda (sensor-name datatype value)
               (printf "No Insert: (~a) ~a=~a~n" datatype sensor-name value))
             field))))

(define (make-stream port)
  (stream-cons
   (with-handlers ([exn:fail? (lambda (exn)
                                (log-inserter-error "Failed to parse packet: ~a" exn)
                                (void))])
     (layer7-read-ebus port))
   (make-stream port)))

(define (main)
  ;; Parse commandline
  (command-line
   #:once-each
   ["--insert" "Do Insert into Database"
               (insert? #t)]
   ["--influx-url" url "Influx server http write url"
                   (influx-url? url)]
   ["--ebus-xml" ebus-xml-path "Influx server http write url"
                 (ebus-xml-path? ebus-xml-path)])
  
  (parameterize ([layer7-definition (layer7-read-ebus-xml (ebus-xml-path?))])
    ;; process ebus packets from stdin
    (for ([packet (make-stream (current-input-port))])
      (when (not (or (void? packet) (eof-object? packet)))
        (handle-packet packet))
      (when (eof-object? packet)
        (exit 1)))))

(exit (main))
