#lang racket/base

(require racket/contract
         racket/match)

(provide (contract-out
          [solve-1 (-> string? integer?)]
          [solve-2 (-> string? integer?)]))

(define (rot-dir dir rot)
  (define (rotl dir) (cond
                       [(eq? dir '+x) '+y]
                       [(eq? dir '+y) '-x]
                       [(eq? dir '-x) '-y]
                       [(eq? dir '-y) '+x]))
  (define (rotr dir) (cond
                       [(eq? dir '+x) '-y]
                       [(eq? dir '-y) '-x]
                       [(eq? dir '-x) '+y]
                       [(eq? dir '+y) '+x]))
  (cond
    [(char=? #\L rot) (rotl dir)]
    [(char=? #\R rot) (rotr dir)]))

(define (move p dir dist)
  (match-define (cons x y) p)
  (cond
    [(eq? dir '+x) (cons (+ x dist) y)]
    [(eq? dir '-x) (cons (- x dist) y)]
    [(eq? dir '+y) (cons x (+ y dist))]
    [(eq? dir '-y) (cons x (- y dist))]))

(define (blocks pt)
  (+ (abs (car pt)) (abs (cdr pt))))

(define (input path)
  (call-with-input-file path
    (lambda (inp) (regexp-split ", " (read-line inp)))))

(define (solve-1 path)
  (define-values (_ dest)
    (for/fold ([dir '+y]
               [pos '(0 . 0)])
              ([i (input path)])
      (values (rot-dir dir (string-ref i 0))
              (move pos dir (string->number (substring i 1))))))
  (blocks dest))

(define (cross-pt pt pts)
  (match-define (cons x y) pt)
  (match-define (list (cons x/ y/) r-pts ...) pts)
  (define/match (loop pts)
    [((list p1 p2 r-pts ...))
     (match-let ([(cons x1 y1) p1]
                 [(cons x2 y2) p2])
       (cond
         [(= x x/)
          (unless (= y1 y2) (error 'pts))
          (if (and (>= x (min x1 x2)) (<= x (max x1 x2))
                   (< (min y y/) y1) (>= (max y y/) y1))
              (cons x y1)
              (loop r-pts))]
         [else
          (unless (= x1 x2) (error 'pts))
          (if (and (>= y (min y1 y2)) (<= y (max y1 y2))
                   (< (min x x/) x1) (>= (max x x/) x1))
              (cons x1 y)
              (loop r-pts))]))]
    [(_) (void)])
  (unless (null? r-pts)
    (loop (cdr r-pts))))

(define (solve-2 path)
  (define in (input path))
  (define (loop in dir pts)
    (match in
      [(list i in-rest ...)
       (let* ([dir (rot-dir dir (string-ref i 0))]
              [cur (move (car pts) dir (string->number (substring i 1)))]
              [pt (cross-pt cur pts)])
         (cond
           [(void? pt) (loop in-rest dir (cons cur pts))]
           [else (blocks pt)]))]
      [_ (void)]))
  (loop in '+y (list '(0 . 0))))
