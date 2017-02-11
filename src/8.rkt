#lang racket/base

(require racket/contract
         racket/list
         racket/match
         racket/string)

(provide (contract-out
          [solve-1 (-> string? integer?)]
          [solve-2 (-> string? void?)]))

(module screen-ops typed/racket
  (require math/array)

  (provide eval-cmd
           count-lit-pixels
           show-screen
           clear-screen)

  (: make-screen (->* (Integer Integer) (#:init Fixnum) (Settable-Array Fixnum)))
  (define (make-screen width height #:init [init 0])
    (array->mutable-array (make-array (vector height width) init)))

  (define w 50)
  (define h 6)
  (define screen (make-screen w h))

  (: eval-cmd (-> (U (List 'rect Integer Integer)
                     (List 'rotrow Integer Integer)
                     (List 'rotcol Integer Integer))
                  Void))
  (define (eval-cmd cmd)
    (match cmd
      [(list 'rect r1 r2)
       (array-slice-set! screen (list (:: #f r2) (:: #f r1)) (array 1))]
      [(list 'rotrow r1 r2)
       (define rot (remainder r2 w))
       (define falling-pixels (array-slice-ref screen (list r1 (:: (- w rot) #f))))
       (array-slice-set! screen (list r1 (:: rot #f))
                         (array-slice-ref screen (list r1 (:: #f (- w rot)))))
       (array-slice-set! screen (list r1 (:: #f rot))
                         falling-pixels)]
      [(list 'rotcol r1 r2)
       (define rot (remainder r2 h))
       (define falling-pixels (array-slice-ref screen (list (:: (- h rot) #f) r1)))
       (array-slice-set! screen (list (:: rot #f) r1)
                         (array-slice-ref screen (list (:: #f (- h rot)) r1)))
       (array-slice-set! screen (list (:: #f rot) r1)
                         falling-pixels)]))

  (: count-lit-pixels (-> Integer))
  (define (count-lit-pixels)
    (array-count positive? screen))

  (: show-screen (-> Void))
  (define (show-screen)
    (for ([i (in-range h)])
      (for ([j (in-range w)])
        (display (if (= 0 (array-ref screen (vector i j)))
                     #\space
                     #\u2588)))
      (displayln "")))

  (: clear-screen (-> Void))
  (define (clear-screen)
    (set! screen (make-screen w h))))

(require 'screen-ops)

(define (read-eval str)
  (define regexps (list '(rect . #px"rect (\\d+)x(\\d+)")
                        '(rotrow . #px"rotate row y=(\\d+) by (\\d+)")
                        '(rotcol . #px"rotate column x=(\\d+) by (\\d+)")))
  (define cmd (for/or ([r regexps])
                (parse-cmd str r)))
  (when cmd (eval-cmd cmd)))

(define (parse-cmd str cmd-string)
  (cond
    [(regexp-match (cdr cmd-string) str) => (lambda (m)
                                              (if (= (string-length str)
                                                     (string-length (first m)))
                                                  (list (car cmd-string)
                                                        (string->number (second m))
                                                        (string->number (third m)))
                                                  #f))]
    [else #f]))

(define (solve path)
  (with-input-from-file path
    (lambda ()
      (for ([l (in-lines)])
        (read-eval l)))))

(define (solve-1 path)
  (clear-screen)
  (solve path)
  (count-lit-pixels))

(define (solve-2 path)
  (clear-screen)
  (solve path)
  (show-screen))
