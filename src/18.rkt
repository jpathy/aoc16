#lang racket/base

(require racket/contract
         racket/match)

(provide (contract-out
          [solve-1 (-> string? integer?)]
          [solve-2 (-> string? integer?)]))

(define (solve path n)
  (with-input-from-file path
    (lambda ()
      (define (in-rev-3s str-list)
        (let loop ([s (cons #\. str-list)]
                   [n '()])
          (match s
            [(list l c r _ ...)
             (loop (cdr s) (cons (list l c r) n))]
            [(list l c)
             (cons (list l c #\.) n)])))
      (for/fold ([count 0]
                 [str (string->list (read-line))])
                ([i (in-range n)])
        (define-values (cnt next) (for/fold ([cnt count]
                                             [s '()])
                                            ([e (in-list (in-rev-3s str))])
                                    (match-define (list l c r) e)
                                    (values (if (char=? c #\.)
                                                (add1 cnt)
                                                cnt)
                                            (cons (if (char=? l r)
                                                      #\.
                                                      #\^)
                                                  s))))
        (values cnt next)))))

(define (solve-1 path)
  (define-values (c _) (solve path 40))
  c)

(define (solve-2 path)
  (define-values (c _) (solve path 400000))
  c)
