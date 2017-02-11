#lang racket/base

(require  racket/contract
          openssl/md5)

(provide (contract-out
          [solve-1 (-> string? string?)]
          [solve-2 (-> string? string?)]))

(define (solve-1 input)
  (define out
    (for*/vector #:length 8
                 ([i (in-naturals)]
                  [h (in-value (md5-bytes (open-input-string (format "~a~a" input i))))]
                  #:when (and (= (bytes-ref h 0) 0)
                              (= (bytes-ref h 1) 0)
                              (= (quotient (bytes-ref h 2) 16) 0)))
      (remainder (bytes-ref h 2) 16)))
  (apply string-append
         (map (lambda (x) (format "~x" x)) (vector->list out))))

(define (solve-2 input)
  (define count 0)
  (define out
    (for*/fold ([out (make-vector 8 16)])
               ([i (in-naturals)]
                [h (in-value (md5-bytes (open-input-string (format "~a~a" input i))))]
                [idx (in-value (remainder (bytes-ref h 2) 16))]
                #:break (= count 8)
                #:when (and (= (bytes-ref h 0) 0)
                            (= (bytes-ref h 1) 0)
                            (= (quotient (bytes-ref h 2) 16) 0)
                            (< idx 8) (= (vector-ref out idx) 16)))
      (vector-set! out idx (quotient (bytes-ref h 3) 16))
      (set! count (add1 count))
      out))
  (apply string-append
         (map (lambda (x) (format "~x" x)) (vector->list out))))
