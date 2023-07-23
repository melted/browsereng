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

(define (lex body)
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
  (get-output-string output))

(struct display-list (items width height font))
(struct layout-item (x y content))

(define (layout page hmax font)
  (define dc (new bitmap-dc%))
  (when font (send dc set-font font))
  (define-values (hspace vstep desc ef) (send dc get-text-extent " "))
  (let loop ((displist '()) (x 0) (y 0) (rest (string-split page)))
    (if (null? rest)
        (display-list (reverse displist) hmax y font)
        (let* ((word (car rest))
               (hstep (let-values (((w h d e) (send dc get-text-extent word font #t))) w)) 
               (new-x (if (< (+ x hstep) hmax) (+ x hstep hspace) 0))
               (new-y (if (= new-x 0) (+ (* vstep 1.25) y) y)))
          (loop (cons (layout-item x y (car rest)) displist) new-x new-y (cdr rest))))))

(define (draw displ dc offset)
  (define vmax (display-list-height displ))
  (define font (display-list-font displ))
  (define-values (w h) (send dc get-size))
  (define top offset)
  (send dc set-font font)
  (for ((item (display-list-items displ)))
    (match item
      ((layout-item x y (? string? str))
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
          (define-values (w h) (send dc get-size))
          (send canvas set-scroll-range 'vertical (inexact->exact (floor (display-list-height displ))))
          (send canvas set-scroll-page 'vertical h)
          (draw displ dc (send canvas get-scroll-pos 'vertical))))))

(send canvas init-manual-scrollbars #f 1000 4 4 0 0)

(send browser show #t)


(define test-url "https://browser.engineering/examples/xiyouji.html")
(define test-url2 "file:///D:/Niklas/src/browsereng/test/default.html")
(define test-url3 "https://browser.engineering/")
(send browser load test-url2)
