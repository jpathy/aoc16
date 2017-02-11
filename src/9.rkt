#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/match
         racket/port)

(provide (contract-out
          [solve-1 (-> string? integer?)]
          [solve-2 (-> string? integer?)]))

(define (non-ws-char? c)
  (not (or (char=? c #\space)
           (char=? c #\newline))))

(define (non-ws-length port count)
  (for/sum ([c (in-input-port-chars port)]
            [i (in-range count)])
    (if (non-ws-char? c)
        1
        0)))

(define-syntax (decompressed-length stx)
  (syntax-parse stx
    [(_ path:id (~optional (~seq #:recursive b:boolean) #:defaults ([b #'#f])))
     #`(call-with-input-file path
         (lambda (port)
           (define (loop port acc)
             (if (equal? (peek-char port) eof)
                 acc
                 (match (regexp-try-match #px"^\\((\\d+)x(\\d+)\\)" port)
                   [(list m len-s rep-s)
                    (define rep (string->number (bytes->string/utf-8 rep-s)))
                    (define len (string->number (bytes->string/utf-8 len-s)))
                    #,(if (syntax->datum (attribute b))
                          #'(loop port (+ acc (* rep (loop (make-limited-input-port port len) 0))))
                          #'(loop port (+ acc (* rep (non-ws-length port len)))))]
                   [_ (loop port (+ acc (if (non-ws-char? (read-char port)) 1 0)))])))
           (loop port 0)))]))

(define (solve-1 path)
  (decompressed-length path))

(define (solve-2 path)
  (decompressed-length path #:recursive #t))
