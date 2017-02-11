#lang racket/base

(require file/md5
         racket/contract
         racket/generator
         racket/list
         racket/match)

(provide (contract-out
          [solve-1 (-> string? string?)]
          [solve-2 (-> string? integer?)]))

(define (solve input gridsz)
  (define (next-moves path)
    (define (open? c) (>= c (char->integer #\b)))
    (for/fold ([l '()])
              ([c (subbytes (md5 (open-input-string (string-append input path)))
                             0 4)]
               [d '(#\U #\D #\L #\R)])
      (if (open? c)
          (cons d l)
          l)))
  (define (in-grid? p)
    (and (>= (car p) 0) (>= (cdr p) 0)
         (< (car p) gridsz) (< (cdr p) gridsz)))
  (define (dirs->cells cur dirs)
    (define-values (path x y) (values (car cur) (cadr cur) (cddr cur)))
    (filter-map (lambda (d)
                  (define (p->c p)
                    (and (in-grid? p) (cons (string-append path (string d)) p)))
                  (match d
                    [#\U (p->c (cons x (sub1 y)))]
                    [#\D (p->c (cons x (add1 y)))]
                    [#\R (p->c (cons (add1 x) y))]
                    [#\L (p->c (cons (sub1 x) y))]))
                dirs))
  (define end (cons (sub1 gridsz) (sub1 gridsz)))

  (define (push! stk e) (set-box! stk (cons e (unbox stk))))
  (define (in-stack stk)
    (in-generator
     (let loop ([s stk])
       (define l (unbox s))
       (unless (empty? l)
         (set-box! s (cdr l))
         (yield (car l))
         (loop s)))))

  (define dfs-stk (box '()))
  (push! dfs-stk (cons "" '(0 . 0)))
  (for/fold ([min ""]
             [max ""])
            ([e (in-stack dfs-stk)])
    (define path (car e))
    (cond
      [(equal? (cdr e) end)
       (values
        (if (= (string-length min) 0)
            path
            (if (< (string-length path)
                   (string-length min))
                path
                min))
        (if (> (string-length path)
               (string-length max))
            path
            max))]
      [else
       (for-each (lambda (cell)
                   (push! dfs-stk cell))
                 (dirs->cells e (next-moves path)))
       (values min max)])))

(define (solve-1 input)
  (define-values (min max) (solve input 4))
  min)

(define (solve-2 input)
  (define-values (min max) (solve input 4))
  (string-length max))
