#lang racket

(require "../lib/loading.rkt")
(require "../lib/utils.rkt")
(require racket/gui/base)

(define browser-window%
  (class frame%
    (super-new)))

(define web-canvas%
  (class canvas%
    (field (page #f))
    (define/public (load site)
      (define body (get-page site))
      (define txt (lex body))
      (set! page txt))
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
  
(define canvas
  (new web-canvas%
   (parent browser)
   (paint-callback
    (λ (canvas dc)
      (for ((ch (get-field page canvas)) (i (in-naturals)))
        (let ((x (* 18 (remainder i 43)))
              (y (* 18 (quotient i 43))))
          (send dc draw-text (string ch) x y)))))))

(send browser show #t)



(define test-url "https://browser.engineering/examples/xiyouji.html")

(send canvas load test-url)
