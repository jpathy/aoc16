#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/syntax
                     racket/vector
                     syntax/parse)
         control)

(provide solve-1 solve-2)

(struct cpu (a b c d) #:mutable #:transparent)

(define-for-syntax (parse-instrs path)
  (with-input-from-file path
    (lambda ()
      (for/vector ([l-no (in-naturals)]
                   [l (in-lines)])
        (match l
          [(regexp #px"cpy (a|b|c|d|-?\\d+) (a|b|c|d)" (list _ s d))
           (let ([s-n (string->number s)])
             (if s-n (list 'cpy s-n d) (list 'cpy s d)))]
          [(regexp #px"inc (a|b|c|d)" (list _ r))
           (list 'inc r)]
          [(regexp #px"dec (a|b|c|d)" (list _ r))
           (list 'dec r)]
          [(regexp #px"jnz (a|b|c|d|-?\\d+) (-?\\d+)" (list _ v off))
           (let ([v-n (string->number v)]
                 [off-n (string->number off)])
             (when (< (+ l-no off-n) 0)
               (error (format "jnz offset : ~a invalid in line ~a" off-n l-no)))
             (if v-n (list 'jnz v-n off-n) (list 'jnz v off-n)))]
          [else (error (format "unknown instruction \"~a\" in line ~a" l l-no))])))))

;; unsafe because we should make sure no jnz pointing in-between replaced code.
;; in that case : patch old code in the end(notably any jnz) and fix corresponding offensive jnzs.
;; unnecessary work for this problem.
(define-for-syntax (unsafe-optimize instrs)
  (define n (vector-length instrs))
  (define result (make-vector n))
  (define (instr-slice i j)
    (vector-copy instrs (if (< i 0) 0 i) (if (> j n) n j)))
  (define (loop i rn)
    (match (instr-slice i (+ i 3))
      [(vector (list 'inc x) (list 'dec y) (list 'jnz y -2))
       (vector-set! result rn (list 'add x y))
       (vector-set! result (add1 rn) (list 'cpy 0 y))
       (vector-set! result (+ 2 rn) (list 'nop))
       (loop (+ i 3) (+ 3 rn))]
      [_
       (if (< i n)
           (begin
             (vector-set! result rn (vector-ref instrs i))
             (loop (add1 i) (add1 rn)))
           rn)]))
  (loop 0 0)
  result)

(define-syntax (compile-instrs stx)
  (syntax-parse stx
    [(_ path:str this-cpu:id)
     (with-handlers ([exn:fail? (lambda (e)
                                  (raise-syntax-error #f (exn-message e) stx #'path))])
       (let ([instrs (unsafe-optimize (parse-instrs (syntax->datum #'path)))])
         #`(tagged-begin
            #,@(flatten
                (for/list ([n (in-naturals)]
                           [i instrs])
                  (define (tag-string n) (format-id stx "instr-~a" n))
                  (define (get r) #`(#,(format-id stx "cpu-~a" r) this-cpu))
                  (define (set r v) #`(#,(format-id stx "set-cpu-~a!" r) this-cpu #,v))
                  (list (tag-string n)
                        (match i
                          [(list 'nop)
                           #'(void)]
                          [(list 'cpy s d)
                           (set d (if (string? s)
                                      (get s)
                                      s))]
                          [(list 'inc r)
                           (set r #`(add1 #,(get r)))]
                          [(list 'dec r)
                           (set r #`(sub1 #,(get r)))]
                          [(list 'jnz v off)
                           (let ([go-stx #`(go #,(tag-string (+ n off)))])
                             (if (number? v)
                                 (if (= v 0) #'(void) go-stx)
                                 #`(unless (= #,(get v) 0) #,go-stx)))]
                          [(list 'add s d)
                           (set s #`(+ #,(get s) #,(get d)))])))))))]))

(define-syntax-rule (solve-1 path)
  (let ([cpu-inst (cpu 0 0 0 0)])
    (compile-instrs path cpu-inst)
    (cpu-a cpu-inst)))

(define-syntax-rule (solve-2 path)
  (let ([cpu-inst (cpu 0 0 1 0)])
    (compile-instrs path cpu-inst)
    (cpu-a cpu-inst)))
