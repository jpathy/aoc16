#lang racket/base

(require data/queue
         math/array
         racket/contract
         racket/list
         racket/set
         racket/vector)

(provide (contract-out
          [solve-1 (-> string? integer?)]
          [solve-2 (-> string? integer?)]))

(define (parse-input path)
  (with-input-from-file path
    (lambda ()
      (let ([map (array-list->array (for/list ([l (in-lines)])
                                      (list->array
                                       (map (lambda (s)
                                              (or (string->number s)
                                                   (string-ref s 0)))
                                            (regexp-match* #px"(\\d+|\\.|\\#)" l)))))])
        (values (for/set ([idx (in-array-indexes (array-shape map))]
                           #:when (number? (array-ref map idx)))
                  idx)
                map)))))

(define (bfs-steps input start locs)
  (define (gate? p)
    (let ([v (array-ref input p)])
      (or (number? v)
          (char=? v #\.))))
  (define (next-moves p)
    (filter (lambda (idxs)
              (and (for/and ([i idxs]
                             [j (array-shape input)])
                     (and (>= i 0) (< i j)))
                   (gate? idxs)))
            (map (lambda (idxs)
                   (vector-map + idxs p))
                 (list '#(0 1) '#(0 -1) '#(1 0) '#(-1 0)))))
  (define seen (mutable-set))
  (define bfs-q (make-queue))
  (enqueue! bfs-q (cons 0 start))
  (set-add! seen start)
  (for/fold ([h (make-immutable-hash)])
            ([e (in-queue bfs-q)]
             #:break (= (set-count locs) (hash-count h))
             #:when (let ([steps (car e)]
                          [idxs (cdr e)])
                      (for-each (lambda (p) (unless (set-member? seen p)
                                              (enqueue! bfs-q (cons (add1 steps) p))
                                              (set-add! seen p)))
                                (next-moves idxs))
                      (number? (array-ref input idxs))))
    (hash-set h (cdr e) (car e))))

(define (n-bfs-steps path)
  (define-values (locs input) (parse-input path))
  (values
   (set-count locs)
   (for*/hash ([p locs]
               [(l steps) (in-hash (bfs-steps input p locs))])
     (values (set (array-ref input p) (array-ref input l)) steps))))

(define (solve-1 path)
  (define-values (vertices dists) (n-bfs-steps path))
  (for/fold ([tsp-dist 0])
            ([l (in-permutations (range 1 vertices))])
    (define-values (dist _)
      (for/fold ([dist 0]
                 [path (cons 0 l)])
               ([i (in-range (sub1 vertices))])
       (values (+ dist (hash-ref dists (set (car path) (cadr path))))
               (cdr path))))
    (cond
      [(or (= tsp-dist 0) (< dist tsp-dist)) dist]
      [else tsp-dist])))

(define (solve-2 path)
  (define-values (vertices dists) (n-bfs-steps path))
  (for/fold ([tsp-dist 0])
            ([l (in-permutations (range 1 vertices))])
    (define-values (dist _)
      (for/fold ([dist 0]
                 [path (append (cons 0 l) (list 0))])
                ([i (in-range vertices)])
        (values (+ dist (hash-ref dists (set (car path) (cadr path))))
                (cdr path))))
    (cond
      [(or (= tsp-dist 0) (< dist tsp-dist)) dist]
      [else tsp-dist])))
