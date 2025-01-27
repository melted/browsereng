#lang racket

(require json)

(define entities
  (with-input-from-file "/home/niklas/repos/browsereng/entities.json"
   (λ () (read-json))))

(with-output-to-file "entities.rkt"
  (λ ()
    (display "#lang racket\n\n")
    (display "(provide named-entities)\n")
    (display "(define named-entities #hash(")
    (for (((k v) (in-hash entities)))
      (display "(")
      (write (symbol->string k))
      (display " . ")
      (write (hash-ref v 'characters))
      (display ")\n"))
    (display "))")))

