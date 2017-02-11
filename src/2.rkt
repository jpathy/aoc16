#lang racket/base

(require racket/contract
         racket/format)

(provide (contract-out
          [solve-1 (-> string? integer?)]
          [solve-2 (-> string? string?)]))

(define (key-1? k)
  (and (exact-integer? k)
       (>= k 1)
       (<= k 9)))

(define/contract (move-1 key dir)
  (-> key-1? char? key-1?)
  (cond
    [(char=? dir #\U) (if (<= key 3) key (- key 3))]
    [(char=? dir #\D) (if (>= key 7) key (+ key 3))]
    [(char=? dir #\R) (if (= (remainder key 3) 0) key (add1 key))]
    [(char=? dir #\L) (if (= (remainder key 3) 1) key (sub1 key))]
    [else (error 'dir)]))

(define (solve-1 path)
  (with-input-from-file path
    (lambda ()
      (define-values (result _)
        (for/fold ([out 0]
                   [cur-key 5])
                  ([in (in-lines)])
          (define l-key (for/fold ([key cur-key])
                                  ([dir (in-string in)])
                          (move-1 key dir)))
          (values (+ (* out 10) l-key) l-key)))
      result)))

(define (key-2? k)
  (and (exact-integer? k)
       (>= k 1)
       (<= k 13)))

(define (move-2 key dir)
  (-> key-2? char? key-2?)
  (let ([d-list '(1 2 3 4 6 7 8 11)]
        [u-list '(3 6 7 8 10 11 12 13)]
        [r-list '(2 3 5 6 7 8 10 11)]
        [l-list '(3 4 6 7 8 9 11 12)])
    (cond
      [(char=? dir #\U) (cond [(= key 13) 11]
                              [(= key 3) 1]
                              [(member key u-list) (- key 4)]
                              [else key])]
      [(char=? dir #\D) (cond [(= key 11) 13]
                              [(= key 1) 3]
                              [(member key d-list) (+ key 4)]
                              [else key])]
      [(char=? dir #\R) (cond [(member key r-list) (+ key 1)]
                              [else key])]
      [(char=? dir #\L) (cond [(member key l-list) (- key 1)]
                              [else key])]
      [else (error 'dir)])))

(define (solve-2 path)
  (with-input-from-file path
    (lambda ()
      (define-values (result _)
        (for/fold ([out '()]
                   [cur-key 5])
                  ([in (in-lines)])
          (define l-key (for/fold ([key cur-key])
                                  ([dir (in-string in)])
                          (move-2 key dir)))
          (values (cons l-key out) l-key)))
      (string-upcase (for/fold ([str ""])
                               ([k result])
                       (string-append (~r k #:base 16) str))))))
