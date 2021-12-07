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

; part2: mean
(define (part2 xs)
  (let ([mean (/ (apply + xs) (length xs))]
	[f (lambda (m x) (let ([xm (abs (- x m))]) (* xm (add1 xm))))]
	[fuel (lambda (m) (/ (apply + (map (lambda (x)
					     (let ([xm (abs (- x m))]) (* xm (add1 xm))))
					   xs))
			     2))])
    (min (fuel (floor mean)) (fuel (ceiling mean)))))

(printf "part 2: ~a\n"
	(with-data part2))
