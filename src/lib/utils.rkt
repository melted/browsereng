#lang racket

(provide read-until read-entity convert-entity)


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
  (let ([entity (string-downcase (read-until port #\;))]) (convert-entity entity)))