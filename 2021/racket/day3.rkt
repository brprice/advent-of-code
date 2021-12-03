#lang racket

(define (with-data f)
  (call-with-input-file
    "../data/day3"
    (lambda (p) (f (sequence->stream (sequence-map string->bits (in-lines p)))))))

(define (with-test-data f)
  (f (map string->bits
	  (list
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
            "01010"))))

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

(define (majority-bits s #:eq [def 0])
  (match (fold-parallel
	   ([pointwise-sum (in-cycle (in-value 0))]
	    [(lambda (l _) (+ l 1)) 0])
	   s)
	 [(list count-1s lines)
	  (sequence-map (lambda (c1) (let ([c0 (- lines c1)])
				       (cond
					 [(> c1 c0) 1]
					 [(< c1 c0) 0]
					 [else def])))
			count-1s)]))

(define (bits->number bs)
  (sequence-fold (lambda (acc b) (+ (* 2 acc) b)) 0 bs))

(define (part1 s)
  (let* ([maj (majority-bits s)]
	 [gamma (bits->number maj)]
	 [delta (bits->number (sequence-map (lambda (b) (- 1 b)) maj))])
    (* gamma delta)))

(printf "part 1: ~a\n"
	(with-data part1))

(define (o2 s)
  (bits->number
    (letrec ([go (lambda (i s)
		   (let* ([maj (majority-bits s #:eq 1)]
			  [sf (sequence-filter
				(lambda (n) (eq? (sequence-ref maj i) (sequence-ref n i)))
				s)])
		     (if (stream-empty? (stream-rest sf)) (stream-first sf) (go (+ 1 i) sf))))])
      (go 0 s))))

(define (co2 s)
  (bits->number
    (letrec ([go (lambda (i s)
		   (let* ([maj (majority-bits s #:eq 1)]
			  [sf (sequence-filter
				(lambda (n) (eq? (- 1 (sequence-ref maj i)) (sequence-ref n i)))
				s)])
		     (if (stream-empty? (stream-rest sf)) (stream-first sf) (go (+ 1 i) sf))))])
      (go 0 s))))

; part 2: we don't worry about being efficient at all
(define (part2 s)
  (* (o2 s) (co2 s)))

(printf "part 2: ~a\n"
	(with-data part2))
