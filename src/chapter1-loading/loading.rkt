#lang racket

(require openssl)
(require net/base64)
(require net/uri-codec)
(require file/gunzip)

(struct url (raw scheme user host port path query fragment) #:transparent)

(struct data-content (media-type media-params base64? data) #:transparent)

(define (parse-http-url str)
  (match-define (list _ scheme _ user host _ port path _ query _ fragment)
    (regexp-match #px"(\\w+)://((\\w+)@)?([\\w\\.\\-]*)(:(\\d+))?(/[^#\\?]*)?(\\?([^#]*))?(#(.*))?"
                  str))
  (url str scheme user host (if port (string->number port) port) (if path path "/") query fragment))

(define (parse-file-url str)
  (match-define (list _ _ _ user host _ port path)
    (regexp-match #px"file:(//((\\w+)@)?([\\w\\.\\-]+)?(:(\\d+))?)?(/.*)$" str))
  (unless path
    (error "File URLs must contain path"))
  (define npath (if (equal? (system-type 'os) 'windows) (string-trim path "/" #:right? #f) path))
  (url str "file" user (if host host "localhost") port npath #f #f))

(define (parse-data-url str)
  (match-define (list data _ media media-attr _ base64 values)
    (regexp-match
     #px"data:(([a-zA-Z0-9\\-.~_/]+)?((;[a-zA-Z0-9\\-.~_]+=[a-zA-Z0-9\\-.~_]+)*)(;base64)?)?,([a-zA-Z0-9\\-.~_%]*)"
     str))
  (url
   str
   "data"
   #f
   #f
   #f
   (data-content media (string-split media-attr ";") (if base64 #t #f) (string->bytes/utf-8 values))
   #f
   #f))

(define (parse-url str)
  (define split (string-split str ":" #:repeat? #f))
  (case (car split)
    [("http" "https") (parse-http-url str)]
    [("file") (parse-file-url str)]
    [("data") (parse-data-url str)]
    [else (error "Unknown URL scheme")]))

(struct response (status headers body) #:transparent)

(define (get-header key headers)
   (let ((kv (assf (λ (k) (string-ci=? k key)) headers)))
     (and kv (cdr kv))))

(define (parse-response resp)
  (define text (open-input-bytes resp))
  (define status-line (read-line text 'return-linefeed))
  (define headers
    (let loop ([acc '()])
      (let ([line (read-line text 'return-linefeed)])
        (if (string=? line "")
            (reverse acc)
            (match-let (((list _ k v) (regexp-match #px"^([\\w\\-]+):(.+)$" line)))
              (loop (cons (cons k (string-trim v)) acc)))))))
  (define gzip? (let ((encoding (get-header "content-encoding" headers)))
                  (and encoding (string-ci=? encoding "gzip"))))
  (define body (if gzip?
                   (let ((inflated (open-output-bytes)))
                     (gunzip-through-ports text inflated)
                     (get-output-bytes inflated))
                   (port->bytes text)))
  (response status-line headers body))

(define (request site)
  (define url (if (url? site) site (parse-http-url site)))
  (define https? (string=? (url-scheme url) "https"))
  (define port (if (url-port url) (url-port url) (if https? 443 80)))
  (define (header key value)
    (format "~a: ~a\r\n" key value))
  (define-values (inport outport)
    (if https? (ssl-connect (url-host url) port 'secure) (tcp-connect (url-host url) port)))
  (fprintf outport "GET ~a HTTP/1.1\r\n" (url-path url))
  (display (header "Host" (url-host url)) outport)
  (display (header "Connection" "close") outport)
  (display (header "Accept-Encoding" "gzip") outport)
  (display "\r\n" outport)
  (flush-output outport)
  (define response (port->bytes inport))
  (close-input-port inport)
  (close-output-port outport)
  (parse-response response))

(define (file-load url)
  (call-with-input-file (url-path url) (λ (port) (port->bytes port))))

(define (data-load url)
  (define content (url-path url))
  (unless (data-content? content)
    (error "Malformed data URL"))
  ((if (data-content-base64? content) base64-decode uri-decode) (data-content-data content)))

(define (read-until port end-char)
  (let loop ([chars '()])
    (let ([ch (read-char port)])
      (if (char=? end-char ch) (list->string (reverse chars)) (loop (cons ch chars))))))


(define (convert-entity entity)
  (cond
      [(string-prefix? entity "#x") (integer->char (string->number (substring entity 2) 16))]
      [(string-prefix? entity "#") (integer->char (string->number (substring entity 1)))]
      [else
       (match entity
         ["gt" #\>]
         ["lt" #\<]
         [else (error "non-supported entity")])]))

(define (read-entity port)
  (let ([entity (string-downcase (read-until port #\;))])
    (convert-entity entity)))

(define (show body)
  (define input (open-input-bytes body))
  (define output (open-output-string))
  (define active-tags (make-hash))
  (define (read-tag)
    (let* ([tag-content (read-until input #\>)] [tag (car (string-split tag-content))])
      (if (char=? (string-ref tag 0) #\/)
          (let ([tag (substring tag 1)])
            (hash-update! active-tags tag sub1)
            (when (= (hash-ref! active-tags tag -1) 0)
              (hash-remove! active-tags tag)))
          (hash-update! active-tags tag add1 1))))
  (define (emit ch)
    (when (hash-has-key? active-tags "body")
      (write-char ch output)))
  (let loop ()
    (let ([ch (read-char input)])
      (if (eof-object? ch)
          void
          (begin
            (case ch
              [(#\<) (read-tag)]
              [(#\&) (let ([entity (read-entity input)]) (emit entity))]
              [else (emit ch)])
            (loop)))))
  (define disp (get-output-string output))
  (display disp))

(define (load site)
  (define url (if (url? site) site (parse-url site)))
  (define body
    (case (url-scheme url)
      [("http" "https")
       (begin
         (match-define (response status headers body) (request site))
         body)]
      [("file") (file-load url)]
      [("data") (data-load url)]))
  (show body))
