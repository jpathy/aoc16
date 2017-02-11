#lang racket/base

(require racket/contract
         racket/match
         racket/string)

(provide (contract-out
          [solve-1 (-> string? integer?)]
          [solve-2 (-> string? integer?)]))

(define (room-real? names cksum)
  (define assoc-tbl (sort
                     (hash->list (for/fold ([h (make-immutable-hash)])
                                           ([c (in-string (apply string-append names))])
                                   (hash-update h c add1 0)))
                     (lambda (x y)
                       (cond
                         [(> (cdr x) (cdr y)) #t]
                         [(= (cdr x) (cdr y)) (char<? (car x)
                                                      (car y))]
                         [else #f]))))
  (string=? cksum (apply string
                         (for/list ([kv assoc-tbl]
                                    [i (in-range 5)])
                           (car kv)))))

(define (solve-1 path)
  (with-input-from-file path
    (lambda ()
      (for/sum ([l (in-lines)])
        (match-let* ([(list _ name cksum) (regexp-match #px"([-\\w]+)\\[(\\w+)\\]" l)]
                     [sep-name (reverse (regexp-split #px"-" name))]
                     [sector (string->number (car sep-name))])
          (if (room-real? (cdr sep-name) cksum) sector 0))))))

(define (rotate name count)
  (define int-a (char->integer #\a))
  (list->string (for/list ([c name])
                  (cond
                    [(char=? c #\-) #\space]
                    [else (integer->char (+ int-a (remainder (+ count (- (char->integer c) int-a)) 26)))]))))

(define (solve-2 path)
  (with-input-from-file path
    (lambda ()
      (for/sum ([l (in-lines)])
        (match-let* ([(list _ name cksum) (regexp-match #px"([-\\w]+)\\[(\\w+)\\]" l)]
                     [sep-name (reverse (regexp-split #px"-" name))]
                     [sector (string->number (car sep-name))])
          (if (and (room-real? (cdr sep-name) cksum)
                   (string-contains? (rotate name sector) "northpole"))
              sector
              0))))))
