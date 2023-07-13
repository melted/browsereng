#lang racket

(require openssl)

(struct url (raw scheme user host port path query fragment) #:transparent)

(define (parse-url str)
  (match-define (list _ scheme _ user host _ port path _ query _ fragment)
    (regexp-match
     #px"(\\w+)://((\\w+)@)?([\\w\\.\\-]*)(:(\\d+))?(/[^#\\?]*)?(\\?([^#]*))?(#(.*))?"
     str))
  (url str scheme user host (if port (string->number port) port) (if path path "/") query fragment))

(struct response (status headers body) #:transparent)

(define (parse-response resp)
  (define text (open-input-bytes resp))
  (define status-line (read-line text 'return-linefeed))
  (define headers
    (let loop ((acc '()))
      (let ((line (read-line text 'return-linefeed)))
        (if (string=? line "")
            (reverse acc)
            (loop (cons line acc))))))
  (define body (port->bytes text))
  (response status-line headers body))

(define (request site)
  (define url (if (url? site) site (parse-url site)))
  (define https? (string=? (url-scheme url) "https"))
  (define port (if (url-port url) (url-port url) (if https? 443 80)))
  (define-values (inport outport) 
    (if https?
        (ssl-connect (url-host url) port 'secure) 
        (tcp-connect (url-host url) port)))
  (fprintf outport "GET ~a HTTP/1.0\r\n" (url-path url))
  (fprintf outport "Host: ~a\r\n\r\n" (url-host url))
  (flush-output outport)
  (define response (port->bytes inport))
  (close-input-port inport)
  (close-output-port outport)
  (parse-response response))

(define (show body)
  (define input (open-input-bytes body))
  (define output (open-output-string))
  (let loop ((in-tag? #f))
    (let ((ch (read-char input)))
      (if (eof-object? ch)
          void
          (if (or in-tag? (char=? ch #\<))
              (loop (not (char=? ch #\>)))
              (begin
                (write-char ch output)
                (loop #f))))))
  (define disp (get-output-string output))
  (display disp))

(define (load site)
  (match-define (response status headers body) (request site))
  (show body))