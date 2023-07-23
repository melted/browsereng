#lang racket

(require "../lib/loading.rkt")
(require "../lib/utils.rkt")
(require racket/gui/base)

(define browser-window%
  (class frame%
    (field (displ #f))
    (field (text #f))
    (field (inner #f))
    (field (font (make-font #:size 15 #:face "Times New Roman")))
    (define/public (load site)
      (define body (get-page site))
      (define txt (lex body))
      (set! text txt)
      (relayout))
    (define/override (on-size w h)
      (relayout))
    (define/public (relayout)
      (when text
        (set! displ (layout text (- (send inner get-width) 100) font))
        (send this refresh)))
    (define/public (resize-font pts)
      (set! font (make-font #:size pts #:face "Times New Roman"))
      (relayout))
    (super-new)))

(define web-canvas%
  (class canvas%
    (field (scroll-x 0))
    (define/override (on-scroll event)
      (send this refresh))
    (define/override (on-char event)
      (define key (send event get-key-code))
      (define pos (send this get-scroll-pos 'vertical))
      (define max-pos (send this get-scroll-range 'vertical))
      (define parent (send this get-parent))
      (match key
        ('up (send this set-scroll-pos 'vertical (max (- pos 100) 0)))
        ('wheel-up (send this set-scroll-pos 'vertical (max (- pos 100) 0)))
        ('down (send this set-scroll-pos 'vertical (min (+ pos 100) max-pos)))
        ('wheel-down (send this set-scroll-pos 'vertical (min (+ pos 100) max-pos)))
        (#\+ (send parent resize-font 40))
        (#\- (send parent resize-font 15))
        (else (void)))
      (send this refresh))
    (super-new)
    (set-field! inner (send this get-parent) this)))

(define browser
  (new browser-window%
       (label "browser")
       (width 800)
       (height 600)))

(struct tag-node (name rest) #:transparent)
(struct text-node (str) #:transparent)

(define (lex body)
  (define input (open-input-bytes body))
  (define (read-tag)
    (let* ([tag-content (read-until input #\>)]
           [t (string-cut tag-content " ")])
      (if tag-content
          (tag-node (car t) (cdr t))
          #f)))
  (define (make-text chars)
    (text-node (list->string (reverse chars))))
  (let loop ((items '()) (chars '()))
    (let ([ch (read-char input)])
      (match ch
        [(? eof-object? c) (if (null? chars)
                               (reverse items)
                               (reverse (cons (make-text chars) items)))]
        [#\< (let ((new-items (if (null? chars)
                                  items
                                  (cons (make-text chars) items)))
                   (tag? (read-tag)))
               (if tag?
                   (loop (cons tag? new-items) '())
                   (loop new-items '())))]
        [#\& (let ([entity (read-entity input)]) (loop items (cons entity chars)))]
        [else (loop items (cons ch chars))]))))

(struct display-list (items width height font) #:transparent)
(struct layout-item (x y content font) #:transparent)

(define (layout page hmax font)
  (define dc (new bitmap-dc%))
  (when font (send dc set-font font))
  (define-values (hspace vstep desc ef) (send dc get-text-extent " "))
  (let loop ((displist '()) (x 0) (y 0) (rest page))
    (if (null? rest)
        (display-list (reverse displist) hmax y font)
        (match (car rest)
          ((text-node str)
           (let loop2 ((dlist displist) (dx x) (dy y) (words (string-split str)))
             (if (null? words)
                 (loop dlist dx dy (cdr rest))
                 (let* ((word (car words))
                  (hstep (let-values (((w h d e) (send dc get-text-extent word font #t))) w)) 
                  (new-x (if (< (+ dx hstep) hmax) (+ dx hstep hspace) 0))
                  (new-y (if (= new-x 0) (+ (* vstep 1.25) dy) dy)))
                   (loop2 (cons (layout-item dx dy word font) dlist) new-x new-y (cdr words))))))
           ((tag-node tg _) (loop displist x y (cdr rest)))))))

(define (draw displ dc offset)
  (define vmax (display-list-height displ))
  (define font (display-list-font displ))
  (define-values (w h) (send dc get-size))
  (define top offset)
  (send dc set-font font)
  (for ((item (display-list-items displ)))
    (match item
      ((layout-item x y (? string? str) f)
       (cond
         ((and (> y (- top 20)) (< y (+ top h 20)))
          (send dc draw-text str x (- y top) #t))))
      (else (void)))))

(define canvas
  (new web-canvas%
       (parent browser)
       (style '(vscroll))
       (paint-callback
        (λ (canvas dc)
          (define displ (get-field displ browser))
          (when displ
            (define-values (w h) (send dc get-size))
            (send canvas set-scroll-range 'vertical (inexact->exact (floor (display-list-height displ))))
            (send canvas set-scroll-page 'vertical h)
            (draw displ dc (send canvas get-scroll-pos 'vertical)))))))

(send canvas init-manual-scrollbars #f 1000 4 4 0 0)

(send browser show #t)


(define test-url "https://browser.engineering/examples/xiyouji.html")
(define test-url2 "file:///D:/Niklas/src/browsereng/test/default.html")
(define test-url3 "https://browser.engineering/")
(send browser load test-url2)
