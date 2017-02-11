#lang racket/base

(require racket/contract
         racket/list
         racket/string)

(provide (contract-out
          [solve-1 (-> string? integer?)]
          [solve-2 (-> string? integer?)]))

(define (abba? str)
  (regexp-match? #px"((.)(?!\\2))(.)\\3\\1" str))

(define (aba-set str)
  (remove-duplicates (regexp-match* #px"(?=(((.)(?!\\3))(.)\\3))"
                                    str #:match-select cadr)))

(define (aba->bab str)
  (string (string-ref str 1) (string-ref str 0) (string-ref str 1)))

(define (solve path pred)
  (with-input-from-file path
    (lambda ()
      (for/sum ([l (in-lines)])
        (let*-values ([(a-list) (regexp-match* #px"([^\\[]+)(?:\\[(.+?)\\])?" l #:match-select cdr)]
                      [(supernet-seq hypernet-seq) (for/fold ([sn-seq '()]
                                                              [hn-seq '()])
                                                             ([p a-list])
                                                     (values (cons (car p) sn-seq)
                                                             (if (cadr p)
                                                                 (cons (cadr p) hn-seq)
                                                                 hn-seq)))])
          (if (pred supernet-seq hypernet-seq) 1 0))))))

(define (solve-1 path)
  (solve path (lambda (sn-seq hn-seq)
                (and (not (ormap abba? hn-seq))
                     (ormap abba? sn-seq)))))

(define (solve-2 path)
  (solve path (lambda (sn-seq hn-seq)
                (define bab-list (for/fold ([set '()])
                                           ([str sn-seq])
                                   (remove-duplicates (append (map aba->bab (aba-set str))
                                                              set))))
                (ormap (lambda (hn-str)
                         (ormap (lambda (bab)
                                  (string-contains? hn-str bab))
                                bab-list))
                       hn-seq))))
