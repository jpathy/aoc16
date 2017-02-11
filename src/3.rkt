#lang racket/base

(require racket/contract
         racket/match)

(provide (contract-out
          [solve-1 (-> string? integer?)]
          [solve-2 (-> string? integer?)]))

(define (triangle? sides)
  (match sides
    [(list a b c)
     (and (> (+ a b) c) (> (+ a c) b) (> (+ b c) a))]
    [_ (error 'sides)]))

(define (solve-1 path)
  (with-input-from-file path
    (lambda ()
      (for/sum ([l (in-lines)])
        (if (triangle? (map string->number
                         (filter (lambda (s) (not (string=? s "")))
                                 (regexp-split #px"\\s+" l))))
            1
            0)))))

(define (solve-2 path)
  (define ((read-n-lines n) port)
    (for/list ([l (in-lines port)]
               [i (in-range n)])
      l))
  (call-with-input-file path
    (lambda (port)
      (for/sum ([3-lines (in-port (read-n-lines 3) port)]
                #:break (null? 3-lines))
        (define triplets (apply map list
                                (for/list ([l 3-lines])
                                  (map string->number
                                       (filter (lambda (s) (not (string=? s "")))
                                               (regexp-split #px"\\s+" l))))))
        (foldl (lambda (t c) (if (triangle? t) (add1 c) c)) 0 triplets)))))
