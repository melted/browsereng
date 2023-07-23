#lang racket

(provide read-until read-entity convert-entity string-find string-cut)


(define (read-until port end-char)
  (let loop ([chars '()])
    (let ([ch (read-char port)])
      (cond
        ((eof-object? ch) #f)
        ((char=? end-char ch)
          (list->string (reverse chars)))
        (else (loop (cons ch chars)))))))


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
  (let ([entity (string-downcase (read-until port #\;))]) (convert-entity entity)))

(define (string-find str contained)
  (regexp-match-positions (regexp-quote contained) str))

(define (string-cut str sep)
  (define pos (string-find str sep))
  (match pos
    (#f (list str ""))
    ((list (cons start end) _ ...)
      (list (substring str 0 start) (substring str end)))))