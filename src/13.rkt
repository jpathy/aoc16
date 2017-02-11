#lang racket/base

(require data/queue
         racket/contract
         racket/set)

(provide (contract-out
          [solve-1 (-> integer? integer?)]
          [solve-2 (-> integer? integer?)]))

(define (open? pt input)
  (define x (car pt))
  (define y (cdr pt))
  (define n (+ (* x x) (* 3 x) (* 2 x y) y (* y y) input))
  (even? (for/sum ([c (number->string n 2)]
                   #:when (char=? c #\1))
           1)))

(define (bfs-steps input start term?-proc for-each-proc)
  (define (open-adj pt)
    (define-values (x y) (values (car pt) (cdr pt)))
    (filter (lambda (p) (and (>= (car p) 0)
                             (>= (cdr p) 0)
                             (open? p input)))
            (list (cons (add1 x) y) (cons (sub1 x) y) (cons x (add1 y)) (cons x (sub1 y)))))
  (define seen (mutable-set))
  (define bfs-q (make-queue))
  (enqueue! bfs-q (cons 0 start))
  (set-add! seen start)
  (for ([elem (in-queue bfs-q)]
        #:break (term?-proc (car elem) (cdr elem)))
    (define steps (car elem))
    (define pt (cdr elem))
    (for-each-proc steps pt)
    (for-each (lambda (p) (unless (set-member? seen p)
                            (enqueue! bfs-q (cons (add1 steps) p))
                            (set-add! seen p)))
              (open-adj pt))))

(define (min-steps start end input)
  (define steps 0)
  (bfs-steps input start
             (lambda (cur-step p)
               (let ([cond (equal? p end)])
                 (if cond
                     (begin
                       (set! steps cur-step)
                       cond)
                     #f)))
             (lambda (cur-step p) (void)))
  steps)

(define (count-pts start steps input)
  (define count 0)
  (define pt-set (mutable-set))
  (bfs-steps input start
             (lambda (cur-step p)
               (= cur-step (add1 steps)))
             (lambda (cur-step p)
               (set! count (add1 count))))
  count)

(define (solve-1 input)
  (min-steps '(1 . 1) '(31 . 39) input))

(define (solve-2 input)
  (count-pts '(1 . 1) 50 input))
