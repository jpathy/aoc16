#lang racket/base

(require racket/contract
         racket/match)

(provide (contract-out
          [solve-1 (-> string? integer?)]
          [solve-2 (-> string? integer?)]))

(define (input path)
  (with-input-from-file path
    (lambda ()
      (for/list ([l (in-lines)])
        (match l
          [(regexp #px"(\\d+)-(\\d+)" (list _ b e))
           (cons (string->number b) (string->number e))]
          [_ (error "invalid input")])))))

(define (solve path)
  (define (merge p1 p2)
    (define (min/max n1 n2)
      (if (< n1 n2) (values n1 n2) (values n2 n1)))
    (define-values (xmin xmax) (min/max (car p1) (car p2)))
    (define-values (ymin ymax) (min/max (cdr p1) (cdr p2)))
    (cond
      [(< (add1 ymin) xmax) #f]
      [else (cons xmin ymax)]))
  (for/fold ([res '()])
            ([p (sort (input path) < #:key car)])
    (let ([m (and (not (null? res))
                  (merge p (car res)))])
      (if m
          (cons m (cdr res))
          (cons p res)))))

(define (solve-1 path)
  (add1 (cdar (reverse (solve path)))))

(define (solve-2 path)
  (- 4294967296
     (for/sum ([p (solve path)])
       (- (cdr p) (sub1 (car p))))))
