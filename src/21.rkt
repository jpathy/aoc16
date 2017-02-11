#lang racket/base

(require racket/contract
         racket/list
         racket/match)

(provide (contract-out
          [solve-1 (-> string? string?)]
          [solve-2 (-> string? string?)]))

(define (swap-n str-box i j)
  (let* ([str (unbox str-box)]
        [x (string-ref str i)])
    (string-set! str i (string-ref str j))
    (string-set! str j x)))

(define (swap-c str-box x y)
  (define str (unbox str-box))
  (for ([i (in-range (string-length str))]
        [c str])
    (cond
      [(char=? c x) (string-set! str i y)]
      [(char=? c y) (string-set! str i x)])))

(define (reverse str-box i j)
  (define n-1 (- j i))
  (for ([k (in-range (quotient (add1 n-1) 2))])
    (swap-n str-box (+ i k) (+ i (- n-1 k)))))

(define (move str-box i j)
  (define str (unbox str-box))
  (cond
    [(< i j) (let ([c (string-ref str i)])
               (string-copy! str i str (add1 i) (add1 j))
               (string-set! str j c))]
    [(> i j) (let ([c (string-ref str i)])
               (string-copy! str (add1 j) str j i)
               (string-set! str j c))]))

(define (rotate str-box k b)
  (let* ([str (unbox str-box)]
         [n (string-length str)]
         [k (remainder k n)])
    (if b
       (let ([tmp (substring str (- n k))])
         (string-copy! str k str 0 (- n k))
         (string-copy! str 0 tmp))
       (let ([tmp (substring str 0 k)])
         (string-copy! str 0 str k)
         (string-copy! str (- n k) tmp)))))

(define (rotate-by-idx str-box c)
  (let ([idx (for/first ([i (in-range (string-length (unbox str-box)))]
                         [e (unbox str-box)]
                     #:when (char=? e c))
               i)])
    (if (>= idx 4)
        (rotate str-box (+ 2 idx) #t)
        (rotate str-box (+ 1 idx) #t))))

(define (solve input path)
  (with-input-from-file path
    (lambda ()
      (define str-box (box (string-copy input)))
      (for ([l (in-lines)])
        (match l
          [(regexp #px"swap position (\\d+) with position (\\d+)" (list _ x y))
           (swap-n str-box (string->number x) (string->number y))]
          [(regexp #px"swap letter (\\w) with letter (\\w)" (list _ x y))
           (swap-c str-box (string-ref x 0) (string-ref y 0))]
          [(regexp #px"rotate (left|right) (\\d+) step" (list _ d s))
           (let ([n (string->number s)])
             (if (string=? d "left")
                 (rotate str-box n #f)
                 (rotate str-box n #t)))]
          [(regexp #px"rotate based on position of letter (\\w)" (list _ s))
           (rotate-by-idx str-box (string-ref s 0))]
          [(regexp #px"reverse positions (\\d+) through (\\d+)" (list _ x y))
           (reverse str-box (string->number x) (string->number y))]
          [(regexp #px"move position (\\d+) to position (\\d+)" (list _ x y))
           (move str-box (string->number x) (string->number y))]))
      (unbox str-box))))

(define (solve-1 path)
  (solve "abcdefgh" path))

(define (solve-2 path)
  (define input "fbgdceah")
  (for/first ([l (in-permutations (string->list input))]
              #:when (string=? (solve (list->string l) path)
                               input))
    (list->string l)))
