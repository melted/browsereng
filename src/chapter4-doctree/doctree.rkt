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


;; TODO: use reencode-ports for converting text encodings
(define (parse body)
  (define state (parser body '()))
  (define txt (open-output-bytes))
  (define (get-txt)
    (bytes->string/utf-8 (get-output-bytes txt #t)))
  (define (inner-text)
    (let ((t (get-txt)))
             (when (non-empty-string? (string-trim t))
              (add-text state t))))
  (define in-tag? #f)
  (for ((c body))
    (match c 
      (#\< (set! in-tag? #t)
           (inner-text))
      (#\> (set! in-tag? #f)
           (add-tag state (get-txt)))
      (_ (write-char c txt))))
  (inner-text)
  (finish state))

(define (add-tag state str)
  (define unfinished (parser-unfinished state))
  (define-values (tag attrs) (split-tag str))
  (cond
    ((string-prefix? tag "/")
       (match unfinished
         ((list el rest ..1)
          (set-node-children! el (reverse (node-children el)))
          (set-node-children! (car rest) (cons el (node-children (car rest))))
          (set-parser-unfinished! state rest))
         (_ (void))))
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
    (when child
      (set-node-children! parent (cons child (node-children parent))))
    parent)
  (when (null? (parser-unfinished state))
      (add-tag state "html"))
  (foldl close #f (parser-unfinished state)))