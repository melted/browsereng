#lang racket

(require openssl)
(require net/base64)
 (require net/uri-codec)

(struct url (raw scheme user host port path query fragment) #:transparent)

(struct data-content (media-type media-params base64? data) #:transparent)

(define (parse-http-url str)
  (match-define (list _ scheme _ user host _ port path _ query _ fragment)
    (regexp-match
     #px"(\\w+)://((\\w+)@)?([\\w\\.\\-]*)(:(\\d+))?(/[^#\\?]*)?(\\?([^#]*))?(#(.*))?"
     str))
  (url str scheme user host (if port (string->number port) port) (if path path "/") query fragment))

(define (parse-file-url str)
  (match-define (list _ _ _ user host _ port path)
    (regexp-match #px"file:(//((\\w+)@)?([\\w\\.\\-]+)?(:(\\d+))?)?(/.*)$" str))
  (unless path (error "File URLs must contain path"))
  (define npath (if (equal? (system-type 'os) 'windows) (string-trim path "/" #:right? #f) path))
  (url str "file" user (if host host "localhost") port npath #f #f))

(define (parse-data-url str)
  (match-define (list data _ media media-attr _ base64 values)
    (regexp-match
     #px"data:(([a-zA-Z0-9\\-.~_/]+)?((;[a-zA-Z0-9\\-.~_]+=[a-zA-Z0-9\\-.~_]+)*)(;base64)?)?,([a-zA-Z0-9\\-.~_%]*)" str))
  (url str "data" #f #f #f
       (data-content media (string-split media-attr ";") (if base64 #t #f) (string->bytes/utf-8 values))
       #f #f))

(define (parse-url str)
  (define split (string-split str ":" #:repeat? #f))
  (case (car split)
    (("http" "https") (parse-http-url str))
    (("file") (parse-file-url str))
    (("data") (parse-data-url str))
    (else (error "Unknown URL scheme"))))

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
  (define url (if (url? site) site (parse-http-url site)))
  (define https? (string=? (url-scheme url) "https"))
  (define port (if (url-port url) (url-port url) (if https? 443 80)))
  (define (header key value)
    (format "~a: ~a\r\n" key value))
  (define-values (inport outport) 
    (if https?
        (ssl-connect (url-host url) port 'secure) 
        (tcp-connect (url-host url) port)))
  (fprintf outport "GET ~a HTTP/1.1\r\n" (url-path url))
  (display (header "Host" (url-host url)) outport)
  (display (header "Connection" "close") outport)
  (display "\r\n" outport)
  (flush-output outport)
  (define response (port->bytes inport))
  (close-input-port inport)
  (close-output-port outport)
  (parse-response response))

(define (file-load url)
  (call-with-input-file (url-path url)
    (λ (port)
      (port->bytes port))))

(define (data-load url)
  (define content (url-path url))
  (unless (data-content? content) (error "Malformed data URL"))
  ((if (data-content-base64? content)
      base64-decode
      uri-decode) (data-content-data content)))

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
  (define url (if (url? site) site (parse-url site)))
  (define body
    (case (url-scheme url)
      (("http" "https")
       (begin
         (match-define (response status headers body) (request site))
         body))
      (("file") (file-load url))))
  (show body))