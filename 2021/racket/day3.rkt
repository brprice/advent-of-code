#lang racket

(define (with-data f)
  (call-with-input-file
    "../data/day3"
    (lambda (p) (f (sequence->stream (in-lines p))))))

(define (with-test-data f)
  (f (list
       "00100"
       "11110"
       "10110"
       "10111"
       "10101"
       "01111"
       "00111"
       "11100"
       "10000"
       "11001"
       "00010"
       "01010")))

(define (string->bits s)
  (sequence-map (lambda (c) (string->number (string c))) s))

; takes the pointwise sum of two sequences, ignoring any excess elements
; when one is longer than the other
(define (pointwise-sum xs ys)
  (sequence-map + (in-parallel xs ys)))

(define-syntax-rule (fold-parallel ([f i] ...) s)
  (sequence-fold
    (lambda (accs x)
      (map (lambda (g acc) (g acc x)) (list f ...) accs))
    (list i ...) s))

(define (part1 s)
  (match (fold-parallel
	   ([pointwise-sum (in-cycle (in-value 0))]
	    [(lambda (l _) (+ l 1)) 0])
	   (sequence-map string->bits s))
	 [(list count-1s lines)
	  (match (sequence-fold (lambda (gd c1)
				  (let* ([c0 (- lines c1)]
					 [maj1 (> c1 c0)]
				         [gb (if maj1 1 0)]
				         [db (- 1 gb)]
				         [g (+ (* 2 (car gd)) gb)]
				         [d (+ (* 2 (cdr gd)) db)])
				    (cons g d)))
				(cons 0 0)
				count-1s)
		 [(cons gamma delta) (* gamma delta)])]))

(printf "part 1: ~a\n"
	(with-data part1))
