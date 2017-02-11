#lang racket/base

(require racket/contract)

(provide (contract-out
          [solve-1 (-> string? string?)]
          [solve-2 (-> string? string?)]))

(define (solve path cmp)
  (with-input-from-file path
    (lambda ()
      (define hts (for/fold ([hts #()])
                            ([l (in-lines)])
                    (cond
                      [(= (vector-length hts) 0)
                       (build-vector (string-length l)
                                     (lambda (i) (make-immutable-hash)))]
                      [(= (vector-length hts) (string-length l))
                       (for/vector ([c l]
                                    [i (in-naturals)])
                         (hash-update (vector-ref hts i) c add1 0))]
                      [else error 'invalid-input])))
      (list->string (for/list ([ht hts])
                      (caar (sort (hash->list ht) cmp #:key cdr)))))))

(define (solve-1 path)
  (solve path >))

(define (solve-2 path)
  (solve path <))
