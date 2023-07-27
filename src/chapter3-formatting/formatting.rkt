#lang racket

(require "../lib/loading.rkt")
(require "../lib/utils.rkt")
(require racket/gui/base)

(define browser-window%
  (class frame%
    (field (displ #f))
    (field (text #f))
    (field (inner #f))
    (field (font "Georgia"))
    (define/public (load site)
      (define body (get-page site))
      (define txt (lex body))
      (set! text txt)
      (relayout))
    (define/override (on-size w h)
      (relayout))
    (define/public (relayout)
      (when text
        (set! displ (layout text (- (send inner get-width) 20) font))
        (send this refresh)))
    (define/public (resize-font pts)
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
(struct layout-item (x y content font style) #:transparent)

(struct layout-state (x y line items font styling dc width) #:transparent #:mutable)

(struct text-metrics (w h desc) #:transparent)

(define (measure state str font)
  (let-values (((w h d e)
                (send (layout-state-dc state)
                      get-text-extent
                      str
                      font
                      #t)))
    (text-metrics w h d)))

(define (layout-flush state)
  (define line (layout-state-line state))
  (unless (null? line)
    (define center? (hash-has-key? (layout-state-styling state) 'center))
    (define (get-metrics i) (measure state (layout-item-content i) (layout-item-font i)))
    (define metrics (map get-metrics line))
    (define x-adjust (if center? (let ((x-end (layout-item-x (car line)))
                                       (x-ext (text-metrics-w (car metrics))))
                                   (quotient (- (layout-state-width state) (+ x-end x-ext)) 2))
                         0))
    (define (ascent m)
      (- (text-metrics-h m) (text-metrics-desc m)))
    (define max-ascent (apply max (map ascent metrics)))
    (define max-descent (apply max (map text-metrics-desc metrics)))
    (define baseline (+ (layout-state-y state) (* 1.25 max-ascent)))
    (define/match (adjust item)
      (((layout-item x y word f style)) (let ((asc (hash-ref! style
                                                              'super
                                                              (ascent (get-metrics item)))))
                                          (layout-item (+ x x-adjust)
                                                       (- baseline asc)
                                                       word
                                                       f style))))
    (define adjusted (map adjust line))
    (set-layout-state-x! state 0)
    (set-layout-state-y! state (+ baseline max-descent))
    (set-layout-state-line! state '())
    (set-layout-state-items! state (append (layout-state-items state) adjusted))))

(define (update-font state)
  (define styling (layout-state-styling state))
  (define face (hash-ref! styling 'face (send (car (layout-state-font state)) get-face)))
  (define size (hash-ref! styling 'size 12))
  (define style (if (hash-has-key? styling 'italic) 'italic 'normal))
  (define weight (if (hash-has-key? styling 'bold) 'heavy 'normal))
  (define font (make-font #:face face #:size size #:style style
                          #:weight weight
                          #:font-list the-font-list))
  (set-layout-state-font! state (cons font (layout-state-font state))))

(define (pop-font state)
  (set-layout-state-font! state (cdr (layout-state-font state))))

(define (fit state word space)
  (define styles (layout-state-styling state))
  (match-define (layout-state x y line items (list font _ ...) _ dc width) state)
  (define (get-width str i)
    (text-metrics-w (measure state (substring str 0 i) font)))
  (define (find-break str dx)
    (for/last (((ch i) (in-indexed str))
               #:when (and (char=? ch #\u00ad) (< (get-width str i) dx)))
      (list (substring str 0 i) (substring str (+ i 1)))))
  (let ((metrics (measure state word font)))
    (when (> (+ x (text-metrics-w metrics)) width)
      (let ((break? (find-break word (- width x))))
        (if break?
            (let ((first (string-append (car break?) (string #\u2010))))
              (fit state first space)
              (layout-flush state)
              (set! word (cadr break?)))
            (layout-flush state))))
    (let ((item (layout-item (layout-state-x state)
                             (layout-state-y state)
                             word
                             font
                             (hash-copy styles))))
      (set-layout-state-line! state (cons item (layout-state-line state)))
      (set-layout-state-x! state (+ (text-metrics-w metrics)
                                    space (layout-state-x state))))))

(define (layout-token token state)
  (define (add n) (λ (i) (+ i n)))
  (define styles (layout-state-styling state))
  (match token
    ((text-node str)
     (let ((space (text-metrics-w (measure state " " (car (layout-state-font state)))))
           (spl (if (hash-has-key? styles 'pre) (string-split str "\n") (string-split str))))
       (for ((word spl))
         (fit state word space))))
    ((tag-node "b" _) (hash-set! styles 'bold #t)
                      (update-font state))
    ((tag-node "/b" _) (hash-remove! styles 'bold)
                       (pop-font state))
    ((tag-node "i" _) (hash-set! styles 'italic #t)
                      (update-font state))
    ((tag-node "/i" _) (hash-remove! styles 'italic)
                       (pop-font state))
    ((tag-node "big" _) (hash-update! styles 'size (add 4) 16)
                        (update-font state))
    ((tag-node "/big" _) (hash-update! styles 'size (add -4) 12)
                         (pop-font state))
    ((tag-node "small" _) (hash-update! styles 'size (add -2) 10)
                          (update-font state))
    ((tag-node "/small" _) (hash-update! styles 'size (add 2) 12)
                           (pop-font state))
    ((tag-node "br" _) (layout-flush state))
    ((tag-node "/p" _) (layout-flush state)
                       (set-layout-state-y! state (+ (layout-state-y state) 15)))
    ((tag-node "h1" _) (layout-flush state)
                       (hash-update! styles 'size (add 10) 22)
                       (hash-set! styles 'bold #t)
                       (hash-set! styles 'center #t)
                       (update-font state))
    ((tag-node "/h1" _) (layout-flush state)
                        (hash-update! styles 'size (add -10) 12)
                        (hash-remove! styles 'bold)
                        (hash-remove! styles 'center)
                        (pop-font state))
    ((tag-node "sup" _) (let ((metric (measure state " " (car (layout-state-font state)))))
                          (hash-set! styles 'super (- (text-metrics-h metric)
                                                      (text-metrics-desc metric)))
                          (hash-update! styles 'size (add -4) 8)
                          (update-font state)))
    ((tag-node "/sup" _) (hash-remove! styles 'super)
                         (hash-update! styles 'size (add 4) 12)
                         (pop-font state))
    ((tag-node "pre" _) (hash-set! styles 'pre #t)
                        (hash-set! styles 'face "Consolas")
                        (update-font state))
    ((tag-node "/pre" _) (hash-remove! styles 'pre)
                         (pop-font state))
    ((tag-node tag _) void)))

(define (layout tokens width face)
  (define dc (new bitmap-dc%))
  (define font (make-font #:face face))
  (define state (layout-state 0 0 '() '() (list font) (make-hash) dc width))
  (for ((t tokens))
    (layout-token t state))
  (layout-flush state)
  (display-list (layout-state-items state) width (layout-state-y state) font))

(define (draw displ dc offset)
  (define vmax (display-list-height displ))
  (define font (display-list-font displ))
  (define-values (w h) (send dc get-size))
  (define top offset)
  (define items (display-list-items displ))
  (send dc set-font font)
  (for ((item items) (i (in-naturals)))
    (match item
      ((layout-item x y (? string? str) f style)
       (cond
         ((and (> y (- top 20)) (< y (+ top h 20)))
          (send dc set-font f)
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
            (send canvas
                  set-scroll-range
                  'vertical
                  (inexact->exact (floor (display-list-height displ))))
            (send canvas set-scroll-page 'vertical h)
            (draw displ dc (send canvas get-scroll-pos 'vertical)))))))

(send canvas init-manual-scrollbars #f 1000 4 4 0 0)

(send browser show #t)


(define test-url "https://browser.engineering/examples/xiyouji.html")
(define test-url2 "file:///D:/Niklas/src/browsereng/test/default.html")
(define test-url3 "https://browser.engineering/")
(send browser load test-url2)
