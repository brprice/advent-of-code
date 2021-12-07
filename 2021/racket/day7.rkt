#lang racket

(define (with-data f)
  (call-with-input-file
    "../data/day7"
    (lambda (p)
      (f (map string->number (string-split (read-line p) ","))))))

(define (with-test-data f)
  (f (list 16 1 2 0 4 2 7 1 2 14)))

; part1: median
(define (part1 xs)
  (let* ([l (length xs)]
	 [ys (sort xs <)]
	 [median (list-ref ys (ceiling (/ l 2)))])
    (apply + (map (lambda (y) (abs (- y median))) ys))))

(printf "part 1: ~a\n"
	(with-data part1))
