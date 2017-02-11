#lang racket/base

(require racket/contract
         racket/generator)

(provide (contract-out
          [solve-1 (-> string? string?)]
          [solve-2 (-> string? string?)]))

(define (make-join-gen)
  (define stream (box (list #f)))
  (define (push v) (set-box! stream (cons v (unbox stream))))
  (generator
   ()
   (yield #f)
   (for ([_ (in-naturals)])
     (push #f)
     (yield #f)
     (for ([c (cdr (unbox stream))])
       (push (not c))
       (yield (not c))))))

(define (in-dragon-curve str)
  (define bstr (map (lambda (c) (if (char=? c #\0) #f #t))
                    (string->list str)))
  (define bstr-rcpl (map not (reverse bstr)))
  (define join-gen (make-join-gen))
  (in-generator
   (for ([_ (in-naturals)])
     (for ([e bstr])
       (yield e))
     (yield (join-gen))
     (for ([e bstr-rcpl])
       (yield e))
     (yield (join-gen)))))

(define (seq-fold proc seq n)
  (for/fold ([count 0])
            ([i (in-range n)]
             [e seq])
    (proc e count)))

(define (checksum-v1 str n)
  (define ratio (bitwise-and n (bitwise-not (sub1 n))))
  (define dragon-seq (in-dragon-curve str))
  (list->string (map (lambda (b)
                       (if b #\1 #\0))
                     (for/list ([i (in-range (quotient n ratio))])
                       (even? (seq-fold (lambda (e count) (if (eq? e #t)
                                                              (add1 count)
                                                              count))
                                        dragon-seq ratio))))))

(define (solve-1 input)
  (checksum-v1 input 272))

(define (solve-2 input)
  (checksum-v1 input 35651584))
