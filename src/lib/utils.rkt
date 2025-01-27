#lang racket
(require "entities.rkt")
(provide read-until
         read-entity
         convert-entity
         string-find
         string-cut)

(define (read-until port end-char)
  (define str (open-output-string))
  (let loop ()
    (let ([ch (read-char port)])
      (cond
        ((eof-object? ch) #f)
        ((char=? end-char ch)
          (write-char ch str)
          (get-output-string str))
        (else (write-char ch str)
              (loop))))))

(define (convert-entity entity)
  (cond
    [(string-prefix? entity "&#x") (integer->char (string->number (substring entity 2) 16))]
    [(string-prefix? entity "&#") (integer->char (string->number (substring entity 1)))]
    [else (hash-ref named-entities entity entity)]))

(define (read-entity port)
  (let ([entity (read-until port #\;)])
    (convert-entity entity)))

(define (string-find str contained)
  (regexp-match-positions (regexp-quote contained) str))

(define (string-cut str sep)
  (define pos (string-find str sep))
  (match pos
    (#f (list str ""))
    ((list (cons start end) _ ...)
      (list (substring str 0 start) (substring str end)))))