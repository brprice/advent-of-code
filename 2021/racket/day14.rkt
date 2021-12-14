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

; part 2: we have to be a bit smarter: just track how many of each sort of pair we have
(define (step-pairs ps rules)
  (let ([ret '()])
    (for ([pc ps])
	 (let* ([p (car pc)]
		[c (cdr pc)]
		[r (rule rules p)])
	   (set! ret (dict-update ret (list (car p) r) (curry + c) 0))
	   (set! ret (dict-update ret (list r (cadr p)) (curry + c) 0))))
    ret))

(define (part2 xs)
  (match xs
	 [(list start rules)
	  (let* ([pairs (map (lambda (g) (cons (car g) (length g))) (group-by identity (for/list ([a start] [b (cdr start)]) (list a b))))]
		 [f (lambda (x) (step-pairs x rules))]
		 [fs (apply compose (make-list 40 f))]
		 [end (fs pairs)]
		 ; NB: every letter will be double-counted, except the first and last
		 ; so start off with one extra of those, so everything is exactly double counted
		 [letter-counts (list (cons (car start) 1) (cons (last start) 1))])
		(for ([pc end])
		     (set! letter-counts (dict-update letter-counts (caar pc) (curry + (cdr pc)) 0))
		     (set! letter-counts (dict-update letter-counts (cadar pc) (curry + (cdr pc)) 0)))
		(let ([counts (map (lambda (lc) (/ (cdr lc) 2)) letter-counts)])
		  (- (apply max counts) (apply min counts))))]))

(printf "part 2: ~a\n"
	(with-data part2))
