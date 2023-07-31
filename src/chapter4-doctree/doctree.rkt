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

(define head-tags (set "base" "basefont" "bgsound" "noscript" "link" "meta"
                       "title" "style" "script"))

(define attr-regex #px"(\\w+)(=('(([^']|\\\\.)*)'|\"(([^\"]|\\\\.)*)\"|(\\w+)))*")

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
  (define-values (tag attrs) (split-tag str))
  (cond
    ((string-prefix? tag "/")
       (match unfinished
         ((list el rest ..1)
          (set-node-children! (car rest) (cons el (node-children (car rest))))
          (set-parser-unfinished! state rest))
         (else (void))))
    (else
      (let* ((parent (if (null? unfinished) #f (car unfinished)))
             (el (element '() parent tag attrs)))
        (set-parser-unfinished! state (cons el unfinished))))))

(define (add-text state str)
  (define parent (car (parser-unfinished state)))
  (define new-node (text '() parent str))
  (set-node-children! parent (cons new-node (node-children parent))))
    

(define (split-tag str)
  (match-define (list tag rest) (string-cut str " "))
  (define attrs
    (for/hash ((m (regexp-match* attr-regex rest #:match-select values)))
      (values (list-ref m 1) (or (list-ref m 6) (list-ref m 4) (list-ref m 3)))))
  (values tag attrs))
      
(define (finish state)
  (define (close parent child)
    (printf "~a ~a" child parent)
    (when child
      (set-node-children! parent (cons child (node-children parent))))
    parent)
  (when (null? (parser-unfinished state))
      (add-tag state "html"))
  (foldl close #f (parser-unfinished state)))