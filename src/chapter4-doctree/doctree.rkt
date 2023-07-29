#lang racket

(require "../lib/loading.rkt")
(require "../lib/utils.rkt")
(require racket/gui/base)

(struct parser (body unfinished) #:transparent #:mutable)

(struct node (children parent) #:transparent #:mutable)

(struct element node (tag attr) #:transparent #:mutable)
(struct text node (t) #:transparent #:mutable)

(define self-closing-tags (set "area" "base" "br" "col" "embed" "hr" "img" "input"
                               "link" "meta" "param" "source" "track" "wbr"))

(define (parse body)
  (define state (parser body '()))
  (define txt '())
  (define (get-txt)
    (let ((str (list->string (reverse txt))))
      (set! txt '())
      str))
  (define in-tag? #f)
  (for ((c body))
    (match c 
      (#\< (set! in-tag? #t)
           (unless (null? txt)
              (add-text state (get-txt))))
      (#\> (set! in-tag? #f)
           (add-tag state (get-txt)))
      (else (set! txt (cons c txt)))))
  (unless (null? txt)
    (add-text state (get-txt)))
  (finish state))


(define (add-tag state str)
  (define unfinished (parser-unfinished state))
  (cond
    ((string-prefix? str "/")
       (match unfinished
         ((list el rest ..1)
          (set-node-children! (car rest) (cons el (node-children (car rest))))
          (set-parser-unfinished! state rest))
         (else (void))))
    (else
      (let* ((parent (if (null? unfinished) #f (car unfinished)))
             (el (element '() parent str "")))
        (set-parser-unfinished! state (cons el unfinished))))))

(define (add-text state str)
  (define parent (car (parser-unfinished state)))
  (define new-node (text '() parent str))
  (set-node-children! parent (cons new-node (node-children parent))))
    

(define (finish state)
  #f) 