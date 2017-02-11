#lang racket/base

(require racket/base
         racket/contract
         racket/match
         racket/vector)

(provide (contract-out
          [solve-1 (-> string? integer?)]
          [solve-2 (-> string? integer?)]))

(struct cpu (a b c d) #:mutable #:transparent)

(define (parse-instrs path)
  (with-input-from-file path
    (lambda ()
      (for/vector ([l-no (in-naturals)]
                   [l (in-lines)])
        (match l
          [(regexp #px"cpy (a|b|c|d|-?\\d+) (a|b|c|d)" (list _ s d))
           (let ([s-n (string->number s)])
             (list 'cpy (or s-n s) d))]
          [(regexp #px"inc (a|b|c|d)" (list _ r))
           (list 'inc r)]
          [(regexp #px"dec (a|b|c|d)" (list _ r))
           (list 'dec r)]
          [(regexp #px"jnz (a|b|c|d|-?\\d+) (a|b|c|d|-?\\d+)" (list _ v off))
           (let ([v-n (string->number v)]
                 [off-n (string->number off)])
             (list 'jnz (or v-n v) (or off-n off)))]
          [(regexp #px"tgl (a|b|c|d|-?\\d+)" (list _ v))
           (let ([v-n (string->number v)])
             (list 'tgl (or v-n v)))]
          [else (error (format "unknown instruction \"~a\" in line ~a" l l-no))])))))

(define (interp instrs cpu-inst)
  (define n (vector-length instrs))
  (define (instr-slice i j)
    (vector-copy instrs (if (< i 0) 0 i) (if (> j n) n j)))
  (define (get r)
    (if (number? r)
        r
        (eval (list (string->symbol (format "cpu-~a" r)) cpu-inst))))
  (define (set r v)
    (unless (number? r)
      (eval (list (string->symbol (format "set-cpu-~a!" r)) cpu-inst v))))
  (define (loop ip)
    (when (< ip n)
      (match (vector-ref instrs ip)
        [(list 'cpy x y)
         (match (instr-slice ip (+ ip 6))
           [(vector (list 'cpy b c) (list 'inc a) (list 'dec c) (list 'jnz c -2) (list 'dec d) (list 'jnz d -5))
            (set a (+ (get a) (* (get d) (get b))))
            (set c 0)
            (set d 0)
            (loop (+ ip 6))]
           [_
            (set y (get x))
            (loop (add1 ip))])]
        [(list 'inc r)
         (match (instr-slice ip (+ ip 3))
           [(vector (list 'inc x) (list 'dec y) (list 'jnz y -2))
            (set x (+ (get x) (get y)))
            (set y 0)
            (loop (+ ip 3))]
           [_
            (set r (add1 (get r)))
            (loop (add1 ip))])]
        [(list 'dec r)
         (match (instr-slice ip (+ ip 3))
           [(vector (list 'dec y) (list 'inc x) (list 'jnz y -2))
            (set x (+ (get x) (get y)))
            (set y 0)
            (loop (+ ip 3))]
           [_
            (set r (sub1 (get r)))
            (loop (add1 ip))])]
        [(list 'jnz x y)
         (if (= (get x) 0)
             (loop (add1 ip))
             (loop (+ (get y) ip)))]
        [(list 'tgl x)
         (let ([t (+ (get x) ip)])
           (when (< t n)
             (match (vector-ref instrs t)
              [(list 'inc x)
               (vector-set! instrs t (list 'dec x))]
              [(list _ x)
               (vector-set! instrs t (list 'inc x))]
              [(list 'jnz x y)
               (vector-set! instrs t (list 'cpy x y))]
              [(list _ x y)
               (vector-set! instrs t (list 'jnz x y))])))
         (loop (add1 ip))])))
  (loop 0))

(define (solve-1 path)
  (let ([cpu-inst (cpu 7 0 0 0)])
    (interp (parse-instrs path) cpu-inst)
    (cpu-a cpu-inst)))

(define (solve-2 path)
  (let ([cpu-inst (cpu 12 0 0 0)])
    (interp (parse-instrs path) cpu-inst)
    (cpu-a cpu-inst)))
