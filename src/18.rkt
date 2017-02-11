#lang racket/base

(require racket/contract
         racket/generator
         racket/match)

(provide (contract-out
          [solve-1 (-> string? integer?)]
          [solve-2 (-> string? integer?)]))

(define (solve path n)
  (with-input-from-file path
    (lambda ()
      (define (in-3s str-list)
        (in-generator #:arity 3
                      (let loop ([s (cons #\. str-list)])
                        (match s
                          [(list l c r _ ...)
                           (yield l c r)
                           (loop (cdr s))]
                          [(list l c)
                           (yield l c #\.)]))))
      (for/fold ([count 0]
                 [str (string->list (read-line))])
                ([i (in-range n)])
        (define-values (cnt next) (for/fold ([cnt count]
                                             [s '()])
                                            ([(l c r) (in-3s str)])
                                    (values (if (char=? c #\.)
                                                (add1 cnt)
                                                cnt)
                                            (cons (if (char=? l r)
                                                      #\.
                                                      #\^)
                                                  s))))
        (values cnt (reverse next))))))

(define (solve-1 path)
  (define-values (c _) (solve path 40))
  c)

(define (solve-2 path)
  (define-values (c _) (solve path 400000))
  c)
