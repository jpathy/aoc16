#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/match
         racket/port)

(provide (contract-out
          [solve-1 (-> string? integer? integer? integer?)]
          [solve-2 (-> string? integer?)]))

(define bot-cmds (make-parameter #f))
(define bot-values (make-parameter #f))
(define output-bins (make-parameter #f))

(struct bot-val (low high) #:transparent)

(struct chip-dest (type id)
  #:guard (lambda (type id dest)
            (cond
              [(and (integer? id)
                    (symbol? type)
                    (or (eq? type 'bot)
                        (eq? type 'out))) (values type id)]
              [else (error dest "invalid")])))

(struct bot-cmd (low-dest high-dest))

(define (parse-input path)
  (define (string->type str)
    (if (string=? str "bot") 'bot 'out))
  (with-input-from-file path
    (lambda ()
      (for ([l (in-lines)])
        (match l
          [(regexp #px"value (\\d+) goes to bot (\\d+)" (list _ val-s bot-id-s))
           (xfer 'bot (string->number bot-id-s) (string->number val-s))]
          [(regexp #px"bot (\\d+) gives low to (bot|output) (\\d+) and high to (bot|output) (\\d+)" (list _ bot-id-s low-type low-id-s high-type high-id-s))
           (hash-set! (bot-cmds)
                      (string->number bot-id-s)
                      (bot-cmd (chip-dest (string->type low-type) (string->number low-id-s))
                               (chip-dest (string->type high-type) (string->number high-id-s))))])))))

(define (xfer type id val)
  (define (give-bot id val)
    (define bot (hash-ref (bot-values) id #f))
    (if bot
        (let ([low (bot-val-low bot)]
              [high (bot-val-high bot)])
          (cond
            [(not low) (hash-set! (bot-values) id (bot-val (min val high) (max val high)))]
            [(not high) (hash-set! (bot-values) id (bot-val (min val low) (max val low)))]
            [else (error "bot ~a has no free slots" id)]))
        (hash-set! (bot-values) id (bot-val val #f))))
  (define (output id val)
    (hash-set! (output-bins) id val))
  (when val (cond
              [(eq? type 'bot) (give-bot id val)]
              [(eq? type 'out) (output id val)])))

(define-syntax (solve stx)
  (syntax-parse stx
    [(_ path:id
        (~optional v1:id #:defaults ([v1 #'#f]))
        (~optional v2:id #:defaults ([v2 #'#f])))
     (with-syntax ([b (and (syntax->datum (attribute v1))
                           (syntax->datum (attribute v2)))])
       #`(begin
           #,(when (syntax->datum #'b)
               #'(define bot #f))
           (parse-input path)
           (for ([_ (in-naturals)]
                 #:break (hash-empty? (bot-values)))
             (hash-for-each (bot-values)
                            (lambda (k v)
                              (define low (bot-val-low v))
                              (define high (bot-val-high v))
                              (when (and low high)
                                (match (hash-ref (bot-cmds) k)
                                  [(bot-cmd (chip-dest low-type low-id) (chip-dest high-type high-id))
                                   (hash-remove! (bot-values) k)
                                   (xfer low-type low-id low)
                                   (xfer high-type high-id high)
                                   #,(when (syntax->datum #'b)
                                       #'(when (and low high
                                                    (= low (min v1 v2))
                                                    (= high (max v1 v2)))
                                           (set! bot k)))]
                                  [_ (error "invalid bot cmd")])))))
           #,(when (syntax->datum #'b) #'bot)))]))

(define-syntax-rule (with-ht-init body ...)
  (parameterize ([bot-values (make-hash)]
                 [bot-cmds (make-hash)]
                 [output-bins (make-hash)])
    (begin body ...)))

(define (solve-1 path v1 v2)
  (with-ht-init (solve path v1 v2)))

(define (solve-2 path)
  (with-ht-init
    (solve path)
    (* (hash-ref (output-bins) 0) (hash-ref (output-bins) 1) (hash-ref (output-bins) 2))))
