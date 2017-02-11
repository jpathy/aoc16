#lang racket/base

(require racket/contract
         racket/match
         racket/vector)

(provide (contract-out
          [print-input (-> string? void?)]))

(define (parse-input path)
  (with-input-from-file path
    (lambda ()
      (define grid (vector #f))
      (define g-x 0)
      (for ([l (in-lines)])
        (match l
          [(regexp #px"/dev/grid/node-x(\\d+)-y(\\d+)\\s*(\\d+)T\\s*(\\d+)T" (list _ x-s y-s total-s used-s))
           (match-define (list x y total used) (map string->number (list x-s y-s total-s used-s)))
           (unless
               (= g-x x) (set! grid (vector-append grid (vector #f))) (set! g-x x))
           (let ([new (vector (cons used total))])
             (if (vector-ref grid x)
                 (vector-set! grid x (vector-append (vector-ref grid x) new))
                 (vector-set! grid x new)))
           ]
          [_ (void)]))
      grid)))

;; (+ (distance to move _(around #) just before the G) (* 5 (- m 2)) 1)
(define (print-input path)
  (define grid (parse-input path))
  (define-values (m n) (values (vector-length grid) (vector-length (vector-ref grid 0))))
  (define max-total 0)
  (for* ([j (in-range n)]
         [i (in-range m)])
    (let* ([e (vector-ref (vector-ref grid i) j)]
           [u (car e)]
           [t (cdr e)])
      (cond
        [(and (= i 0) (= j 0)) (display "O") (set! max-total t)]
        [(and (= j 0) (= i (sub1 m)) (display "G"))]
        [(<= u max-total) (if (= u 0)
                              (display "_")
                              (display "."))
         (set! max-total (max max-total t))]
        [else (display "#")])
      (if (= i (sub1 m))
          (displayln "")
          (display " ")))))
