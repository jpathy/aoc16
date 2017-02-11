#lang racket/base

(require racket/contract
         racket/match
         racket/vector)

(provide (contract-out
          [solve-1 (-> string? integer?)]
          [solve-2 (-> string? integer?)]))

(define (parse-input path)
  (with-input-from-file path
    (lambda ()
      (for/vector ([l (in-lines)])
        (match l
          [(regexp #px"Disc #(\\d+) has (\\d+) positions.*time=0.*at position (\\d+)" (list _ disk npos pos0))
           (map string->number (list disk npos pos0))]
          [_ (error "invalid input")])))))

(define (solve disks)
  (for ([t (in-naturals)])
    (define cond
      (for/and ([in disks])
       (define-values (disk npos pos0) (values (car in) (cadr in) (caddr in)))
       (= 0 (remainder (+ t disk pos0) npos))))
    #:final cond t))

(define (solve-1 path)
  (solve (parse-input path)))

(define (solve-2 path)
  (let ([disks (parse-input path)])
    (solve (vector-append disks (vector (list (add1 (vector-length disks)) 11 0))))))
