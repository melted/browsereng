#lang racket

(require "../lib/loading.rkt")
(require "../lib/utils.rkt")
(require racket/gui/base)

(define browser-window%
  (class frame%
    (field (displ #f))
    (field (text #f))
    (field (text-size 15))
    (define/public (load site)
      (define body (get-page site))
      (define txt (lex body))
      (set! text txt)
      (define l (layout txt (send this get-width) text-size))
      (set! displ l)
      (send this refresh))
    (define/override (on-size w h)
      (relayout))
    (define/public (relayout)
      (when text
        (set! displ (layout text (send this get-width) text-size))
        (send this refresh)))
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
        (#\+ (set-field! text-size parent 36)
             (send parent relayout))
        (#\- (set-field! text-size parent 20)
             (send parent relayout))
        (else (void)))
      (send this refresh))
    (super-new)))

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

(struct display-list (items width height font-size))
(struct layout-item (x y content))

(define (layout page hmax hstep)
  (define vstep (inexact->exact (floor (* hstep 1.33))))
  (let loop ((displist '()) (x 0) (y 0) (rest (string->list page)))
    (if (null? rest)
        (display-list (reverse displist) hmax y hstep)
        (let* ((new-x (if (< (+ x hstep) hmax) (+ x hstep) 0))
               (new-y (if (> x new-x) (+ vstep y) y))
               (ch (car rest)))
          (cond
            ((char=? ch #\newline) (loop displist 0 (+ 20 y) (cdr rest)))
            (else (loop (cons (layout-item x y (car rest)) displist) new-x new-y (cdr rest))))))))

(define (get-replacement-image ch)
  (if (char>? ch #\U10000)
      (begin   
        (let* ((hex (number->string (char->integer ch) 16))
               (file (format  "D:/Niklas/src/browsereng/resources/openmoji/~a.png"
                              (string-upcase hex))))
        (if (file-exists? file)
            (call-with-input-file file read-bitmap)
            #f)))
      #f))
  
  

(define (draw displ dc offset)
  (define vmax (display-list-height displ))
  (define text-size (display-list-font-size displ))
  (define-values (w h) (send dc get-size))
  (define top offset)
  (define font (make-font #:size (floor (* text-size 0.75))))
  (send dc set-font font)
  (for ((item (display-list-items displ)))
    (match item
      ((layout-item x y (? char? ch))
       (cond
         ((and (> y (- top 20)) (< y (+ top h 20)))
          (let ((repl (get-replacement-image ch)))
            (if repl
                (begin
                  (let-values (((old-w old-h) (send dc get-scale)))
                    (send dc set-scale 0.25 0.25)
                    (send dc draw-bitmap repl (* 4 x) (* 4 (- y top)))
                    (send dc set-scale old-w old-h)))
                (send dc draw-text (string ch) x (- y top)))))
         (else (void)))))))

(define canvas
  (new web-canvas%
       (parent browser)
       (style '(vscroll))
       (paint-callback
        (λ (canvas dc)
          (define displ (get-field displ browser))
          (define-values (w h) (send dc get-size))
          (send canvas set-scroll-range 'vertical (display-list-height displ))
          (send canvas set-scroll-page 'vertical h)
          (draw displ dc (send canvas get-scroll-pos 'vertical))))))

(send canvas init-manual-scrollbars #f 1000 4 4 0 0)

(send browser show #t)


(define test-url "https://browser.engineering/examples/xiyouji.html")
(define test-url2 "file:///D:/Niklas/src/browsereng/test/default.html")

(send browser load test-url)
