#lang racket


(define (maybe-prompt)
  (display "> "))


(define (eval-expr tokens history)
  (cond
    [(null? tokens)
     (error "Invalid Expression")]

    
    [(string=? (car tokens) "+")
     (define r1 (eval-expr (cdr tokens) history))
     (define v1 (first r1))
     (define rest1 (second r1))
     (define r2 (eval-expr rest1 history))
     (define v2 (first r2))
     (define rest2 (second r2))
     (list (+ v1 v2) rest2)]

    
    [(string=? (car tokens) "*")
     (define r1 (eval-expr (cdr tokens) history))
     (define v1 (first r1))
     (define rest1 (second r1))
     (define r2 (eval-expr rest1 history))
     (define v2 (first r2))
     (define rest2 (second r2))
     (list (* v1 v2) rest2)]

    
    [(string=? (car tokens) "/")
     (define r1 (eval-expr (cdr tokens) history))
     (define v1 (first r1))
     (define rest1 (second r1))
     (define r2 (eval-expr rest1 history))
     (define v2 (first r2))
     (define rest2 (second r2))
     (if (zero? v2)
         (error "Division by zero")
         (list (/ v1 v2) rest2))]

    
    [(string=? (car tokens) "-")
     (define r1 (eval-expr (cdr tokens) history))
     (define v1 (first r1))
     (define rest1 (second r1))
     (list (- v1) rest1)]

    
    [(regexp-match? #rx"^\\$[0-9]+$" (car tokens))
     (define idx (string->number (substring (car tokens) 1)))
     (if (and idx (> idx 0) (<= idx (length history)))
         (list (list-ref (reverse history) (sub1 idx)) (cdr tokens))
         (error "Invalid history reference"))]

    
    [(string->number (car tokens))
     (list (string->number (car tokens)) (cdr tokens))]

    [else (error "Invalid Expression")]))



(define (eval-loop history)
  (maybe-prompt)
  (define input (read-line))
  (cond
    [(eof-object? input) (void)]
    [(equal? (string-trim input) "quit") (void)]
    [else
     (with-handlers ([exn:fail?
                      (λ (e)
                        (displayln (string-append "Error: " (exn-message e)))
                        (eval-loop history))])
       (define tokens
         (filter (λ (s) (not (string=? s "")))
                 (regexp-split #px"\\s+" input)))
       (define pair (eval-expr tokens history))
       (define val (first pair))
       (define rest (second pair))
       (if (not (null? (filter (λ (t) (not (string=? t ""))) rest)))
           (displayln "Error: Invalid Expression")
           (let* ([valf (real->double-flonum val)]
                  [newhist (cons valf history)]
                  [id (length newhist)])
             (display id) (display ": ") (displayln valf)
             (eval-loop newhist))))]))


(define (main) (eval-loop '()))
(main)
