#lang racket

(define test-data '(4 8))

(define data
  (for/list ([l (in-lines (open-input-file "../data/day21"))])
	    (string->number (cadr (string-split l ": ")))))

(define (part1 data)
  (define (wrap n)
    (add1 (modulo (sub1 n) 10)))
  (define die 1)
  (define (roll-die)
    (let ([d die])
      (set! die (if (eq? d 100) 1 (add1 d)))
      d))
  ; keep the next player to move as the first elt
  (define (step ps)
    (let* ([r (+ (roll-die) (roll-die) (roll-die))]
	   [new-pos (wrap (+ (caar ps) r))])
      (list (cadr ps) (list new-pos (+ new-pos (cadar ps))))))
  (define (go rs ps)
    (let ([new-ps (step ps)]
	  [new-rs (+ 3 rs)])
      (if (<= 1000 (cadadr new-ps))
	(* (cadar new-ps) new-rs)
	(go new-rs new-ps))))
  (go 0 (list (list (car data) 0) (list (cadr data) 0))))

(printf "part 1: ~a\n"
	(part1 data))
