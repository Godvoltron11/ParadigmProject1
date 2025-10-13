#lang racket



(require racket/cmdline)

(define args (vector->list (current-command-line-arguments)))
(define interactive?
  (not (or (member "-b" args)
           (member "--batch" args))))

(define (maybe-prompt)
  (when interactive? (display "> ")))


(define (eval-expr tokens history)
  (cond
    [(null? tokens)
     (error "Invalid Expression")]

    
    [(string=? (car tokens) "+")
     (define res1 (eval-expr (cdr tokens) history))
     (define val1 (first res1))
     (define rest1 (second res1))
     (define res2 (eval-expr rest1 history))
     (define val2 (first res2))
     (define rest2 (second res2))
     (list (+ val1 val2) rest2)]

    
    [(string=? (car tokens) "*")
     (define res1 (eval-expr (cdr tokens) history))
     (define val1 (first res1))
     (define rest1 (second res1))
     (define res2 (eval-expr rest1 history))
     (define val2 (first res2))
     (define rest2 (second res2))
     (list (* val1 val2) rest2)]

    
    [(string=? (car tokens) "/")
     (define res1 (eval-expr (cdr tokens) history))
     (define val1 (first res1))
     (define rest1 (second res1))
     (define res2 (eval-expr rest1 history))
     (define val2 (first res2))
     (define rest2 (second res2))
     (if (zero? val2)
         (error "Division by zero")
         (list (/ val1 val2) rest2))]

    
    [(string=? (car tokens) "-")
     (define res1 (eval-expr (cdr tokens) history))
     (define val1 (first res1))
     (define rest1 (second res1))
     (list (- val1) rest1)]

    
    [(regexp-match? #rx"^\\$[0-9]+$" (car tokens))
     (define idx (string->number (substring (car tokens) 1)))
     (if (and idx (> idx 0) (<= idx (length history)))
         (list (list-ref (reverse history) (sub1 idx)) (cdr tokens))
         (error "Invalid history reference"))]

    
    [(string->number (car tokens))
     (list (string->number (car tokens)) (cdr tokens))]

    [else
     (error "Invalid Expression")]))

;; Main loop
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
                 (regexp-split #rx"\\s+" input)))
       (define result-pair (eval-expr tokens history))
       (define value (first result-pair))
       (define remaining (second result-pair))
       (if (not (null? (filter (λ (t) (not (string=? t ""))) remaining)))
           (displayln "Error: Invalid Expression")
           (let* ([value-fl (real->double-flonum value)]
                  [new-history (cons value-fl history)]
                  [id (length new-history)])
             (if interactive?
                 (begin
                   (display id)
                   (display ": ")
                   (displayln value-fl))
                 (displayln value-fl))
             (eval-loop new-history))))]))

(define (main) (eval-loop '()))
(main)
