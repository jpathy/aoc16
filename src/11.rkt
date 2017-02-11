#lang racket/base

(require data/queue
         racket/contract
         racket/generator
         racket/list
         racket/match
         racket/set
         racket/vector)

(provide (contract-out
          [solve-1 (-> string? integer?)]
          [solve-2 (-> string? integer?)]))

(struct floor (chips gens) #:transparent)

(struct config (E floors) #:transparent)

(struct state (E floor-ids) #:transparent)

(define nelements #f)

(define (parse-input path)
  (with-input-from-file path
    (lambda ()
      (define elements (make-hash))
      (define-values (_ id-gen) (sequence-generate (in-naturals)))
      (begin0
        (config 0
                (for/vector ([l (in-lines)])
                  (define matches (regexp-match* #px"(\\w+)-compatible microchip|(\\w+) generator"
                                                 l #:match-select cdr))
                  (define f (apply floor
                                   (foldl (lambda (m f)
                                            (let ([chips (first f)]
                                                  [gens (second f)]
                                                  [c (first m)]
                                                  [g (second m)])
                                              (define (cons* e s)
                                                (if e
                                                    (set-add s
                                                             (hash-ref! elements e
                                                                        (lambda () (id-gen))))
                                                    s))
                                              (list (cons* c chips) (cons* g gens))))
                                          (list (set) (set))
                                          matches)))
                  (if (stable? f)
                      f
                      (error "invalid input line: ~a" l))))
        (set! nelements (id-gen))))))

(define (final-config nfloors)
  (define n-1 (sub1 nfloors))
  (config n-1
          (vector-append (for/vector ([i (in-range n-1)])
                           (floor (set) (set)))
                         (vector (floor (apply set (range nelements))
                                        (apply set (range nelements)))))))

(define (config->state conf)
  (define (get-floor e accessor)
    (for/first ([i (in-naturals)]
                [f (config-floors conf)]
                #:when (set-member? (accessor f) e))
      i))
  (state (config-E conf)
         (vector-sort (for/vector ([i (in-range nelements)])
                        (cons (get-floor i floor-chips)
                              (get-floor i floor-gens)))
                      (lambda (x y)
                        (or (< (car x) (car y))
                            (and (= (car x) (car y))
                                 (< (cdr x) (cdr y))))))))

(define (stable? floor)
  (or (set-empty? (floor-gens floor))
      (for/and ([c (in-set (floor-chips floor))])
        (set-member? (floor-gens floor) c))))

(define (in-moves floor)
  (define chips (floor-chips floor))
  (define gens (floor-gens floor))
  (in-generator
   (begin
     (for ([c (in-set chips)])
       (yield (cons (set c) (set))))
     (for ([g (in-set gens)])
       (yield (cons (set) (set g))))
     (for ([p (in-combinations (set->list chips) 2)])
       (yield (cons (list->set p) (set))))
     (for ([p (in-combinations (set->list gens) 2)])
       (yield (cons (set) (list->set p))))
     (for* ([c (in-set chips)]
            [g (in-set gens)])
       (yield (cons (set c) (set g)))))))

(define (in-next-configs conf)
  (define e (config-E conf))
  (define floors (config-floors conf))
  (define cur-floor (vector-ref floors e))
  (define (move m f)
    (let ([new-floor (floor (set-union (floor-chips f) (car m))
                            (set-union (floor-gens f) (cdr m)))]
          [cur-floor (floor (set-subtract (floor-chips cur-floor) (car m))
                            (set-subtract (floor-gens cur-floor) (cdr m)))])
      (if (and (stable? new-floor)
               (stable? cur-floor))
          (cons cur-floor new-floor)
          #f)))
  (in-generator
   (for ([m (in-moves cur-floor)])
     (when (< e (sub1 (vector-length floors)))
       (match (move m (vector-ref floors (add1 e)))
         [(cons c n)
          (let ([nfloors (vector-copy floors)])
            (vector-set*! nfloors e c (add1 e) n)
            (yield (config (add1 e) nfloors)))]
         [_ (void)]))
     (when (> e 0)
       (match (move m (vector-ref floors (sub1 e)))
         [(cons c n)
          (let ([nfloors (vector-copy floors)])
            (vector-set*! nfloors e c (sub1 e) n)
            (yield (config (sub1 e) nfloors)))]
         [_ (void)])))))

(define (search-steps init desired)
  (define seen-states (mutable-set))
  (define bfs-q (make-queue))
  (enqueue! bfs-q (cons 1 (in-next-configs init)))
  (for*/last ([elem (in-queue bfs-q)]
              [conf (cdr elem)]
              #:final (equal? conf desired))
    (define moves (car elem))
    (define state (config->state conf))
    (unless (set-member? seen-states state)
      (set-add! seen-states state)
      (enqueue! bfs-q (cons (add1 moves)
                            (in-next-configs conf))))
    moves))

(define (solve-1 path)
  (define init (parse-input path))
  (define desired (final-config (vector-length (config-floors init))))
  (search-steps init desired))

(define (solve-2 path)
  (define init (parse-input path))
  (define extra (set nelements (add1 nelements)))
  (set! nelements (+ 2 nelements))
  (let ([floor0 (vector-ref (config-floors init) 0)])
    (vector-set! (config-floors init)
                 0 (floor (set-union (floor-chips floor0) extra)
                          (set-union (floor-gens floor0) extra))))
  (define desired (final-config (vector-length (config-floors init))))
  (search-steps init desired))
