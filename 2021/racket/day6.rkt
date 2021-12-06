#lang racket

(define (with-data f)
  (call-with-input-file
    "../data/day6"
    (lambda (p)
      (f (map string->number (string-split (read-line p) ","))))))

(define (with-test-data f)
  (f (list 3 4 3 1 2)))

(define (age-map ages)
  (map (lambda (g) (cons (car g) (length g))) (group-by identity ages)))

(define (step am)
  (let* ([x 0]
        [am1 (dict-map am (lambda (a n)
		 (if (zero? a)
		   (begin (set! x n) (cons 8 n))
		   (cons (sub1 a) n))))])
    (dict-update am1 6 (lambda (n) (+ x n)) 0 )))

(define (total-fish steps starting-ages)
  (apply + (map cdr ((apply compose (make-list steps step)) (age-map starting-ages)))))

(define (part1 ns) (total-fish 80 ns))

(printf "part 1: ~a\n"
	(with-data part1))

(define (part2 ns) (total-fish 256 ns))

(printf "part 2: ~a\n"
	(with-data part2))
