#lang racket/base

(require racket/contract
         racket/sequence
         racket/stream)

(provide (contract-out
          [solve-1 (-> integer? integer?)]
          [solve-2 (-> integer? integer?)]))

;; josephus problem
(define (solve n)
  (add1 (* 2 (- n (arithmetic-shift 1 (sub1 (integer-length n)))))))

(define (solve-1 n)
  (define (next-stream n s shift)
    (for/list ([i (in-range n)]
                 [e (sequence-tail s shift)]
                 #:when (even? i))
                e))
  (define (loop n s shift)
    (if (= n 0)
        (sequence-ref s 0)
        (let* ([shift-n (if (even? n) 0 1)]
               [next-n (- (quotient (add1 n) 2) shift-n)])
          (loop next-n
                (next-stream n s shift)
                shift-n))))
  (loop n (in-range 1 (add1 n)) 0))

(define (solve-2 n)
  (define (next-seq n s shift)
    (define (indices n k i)
      (define flag (= i (quotient (+ n (* 3 k)) 2)))
      (stream-cons flag
                   (indices n (if flag (add1 k) k) (add1 i))))
    (for/vector ([i (in-range n)]
                 [e (sequence-tail (in-cycle s) shift)]
                 [flag (in-stream (indices n 0 0))]
                 #:unless flag)
      e))
  (define (loop n s shift)
    (if (= n 1)
        (sequence-ref s 0)
        (let* ([shift-n (ceiling (/ n 3))]
               [next-n (- n shift-n)])
          (loop next-n
                (next-seq n s shift)
                shift-n))))
  (loop n (in-range 1 (add1 n)) 0))
