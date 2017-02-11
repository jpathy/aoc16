#lang racket/base

(require openssl/md5
         racket/contract)

(provide (contract-out
          [solve-1 (-> string? integer?)]
          [solve-2 (-> string? integer?)]))

(define (solve input hash-fn)
  (define hash-cache (make-hash))
  (define (rep-3 s)
    (let ([m (regexp-match #px"(.)\\1\\1" s)])
      (if m (cadr m) m)))
  (define (rep-5? s c-s)
    (regexp-match? (pregexp (string-append "(" c-s ")\\1\\1\\1\\1")) s))
  (define (cache-get! n)
    (hash-ref! hash-cache n (lambda ()
                             (let ([str (hash-fn (format "~a~a" input n))])
                               (cons (rep-3 str) str)))))
  (define (is-key? n)
    (define r (car (cache-get! n)))
    (and r
         (for/or ([i (in-range 1 1001)])
           (rep-5? (cdr (cache-get! (+ n i))) r))))

  (define count 0)
  (for/last ([i (in-naturals)]
             #:break (= count 64))
    (when (is-key? i)
      (set! count (add1 count)))
    (hash-remove! hash-cache i)
    i))

(define (apply-n n f x)
  (define (loop n x)
    (if (= n 0)
        x
        (loop (sub1 n) (f x))))
  (loop n x))

(define (solve-1 input)
  (solve input (lambda (str) (md5 (open-input-string str)))))

(define (solve-2 input)
  (solve input (lambda (str)
                 (apply-n 2017 (lambda (s)
                                 (md5 (open-input-string s))) str))))
