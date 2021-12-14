#lang racket

(define-syntax-rule (push! x xs)
  (set! xs (cons x xs)))

(define (with-data f)
  (call-with-input-file
    "../data/day14"
    (lambda (p)
      (let* ([lines (in-lines p)]
	     [start (string->list (sequence-ref lines 0))]
             [_ (sequence-ref lines 0)] ; drop blank line
             [r (lambda (l) (match (string-split l " -> ")
				   [(list in out) (cons (string->list in) (car (string->list out)))]))]
             [rules (sequence->list (sequence-map r lines))])
	(f (list start rules))))))

(define (with-test-data f)
  (let ([rules '(((C H) . B)
                 ((H H) . N)
                 ((C B) . H)
                 ((N H) . C)
                 ((H B) . C)
                 ((H C) . B)
                 ((H N) . C)
                 ((N N) . C)
                 ((B H) . H)
                 ((N C) . B)
                 ((N B) . B)
                 ((B N) . B)
                 ((B B) . N)
                 ((B C) . B)
                 ((C C) . N)
                 ((C N) . C)) ])
    (f (list '(N N C B) rules))))

(define (rule rules p)
  (cdr (assoc p rules)))


(define (step x rules)
  (let ([ret '()])
    (push! (car x) ret)
    (for ([a x] [b (cdr x)])
	 (push! (rule rules (list a b)) ret)
	 (push! b ret))
    (reverse ret)
  ))

; part 1: brute-force works fine for 10 steps
(define (part1 xs)
  (match xs
	 [(list start rules)
	  (let* ([f (lambda (x) (step x rules))]
		 [fs (apply compose (make-list 10 f))]
		 [res (fs start)]
		 [ls (map (lambda (g) (length g)) (group-by identity res))])
	    (- (apply max ls) (apply min ls)))]))

(printf "part 1: ~a\n"
	(with-data part1))
