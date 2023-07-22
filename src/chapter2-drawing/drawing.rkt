#lang racket

(require "../lib/loading.rkt")
(require "../lib/utils.rkt")
(require racket/gui/base)

(define browser-window%
  (class frame%
    (field (displ #f))
    (define/public (load site)
      (define body (get-page site))
      (define txt (lex body))
      (define l (layout txt))
      (set! displ l))
    (super-new)))

(define web-canvas%
  (class canvas%
    (field (scroll-x 0))
    (define/override (on-scroll event)
      (define pos (send event get-position))
      (set! scroll-x pos)
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

(struct display-list (items width height))
(struct layout-item (x y content))

(define (layout page)
  (define hstep 18)
  (define vstep 18)
  (define hmax 800)
  (let loop ((displist '()) (x 0) (y 0) (rest (string->list page)))
    (if (null? rest)
        (display-list (reverse displist) hmax y)
        (let* ((new-x (if (< (+ x hstep) hmax) (+ x hstep) 0))
               (new-y (if (> x new-x) (+ vstep y) y))
               (ch (car rest)))
          (cond
            ((char=? ch #\newline) (loop displist 0 (+ 20 y) (cdr rest)))
            (else (loop (cons (layout-item x y (car rest)) displist) new-x new-y (cdr rest))))))))

(define (draw displ dc offset)
  (define vmax (display-list-height displ))
  (define-values (w h) (send dc get-size))
  (define top offset)
  (for ((item (display-list-items displ)))
    (match item
      ((layout-item x y (? char? ch))
       (cond
         ((and (> y (- top 20)) (< y (+ top h 20))) (send dc draw-text (string ch) x (- y top)))
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
          (draw displ dc (get-field scroll-x canvas))))))

(send canvas init-manual-scrollbars #f 1000 4 4 0 0)

(send browser show #t)


(define test-url "https://browser.engineering/examples/xiyouji.html")

(send browser load test-url)
