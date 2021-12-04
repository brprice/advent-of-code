#lang racket

(define (stream-group s)
  (let go ([acc null] [s s])
    (if (stream-empty? s)
      (stream (reverse acc))
      (let ([l (stream-first s)]
	    [r (stream-rest s)])
	(if (non-empty-string? l)
	  (go (cons l acc) r)
	  (stream-cons (reverse acc) (go null r)))))))


; Convert input data to a cons cell:
; - car: a list of numbers drawn
; - cdr: a list of bingo cards, each of which is a square list-of-lists
; (we don't worry about trying to stream data through the whole program)
(define (parse-data s)
  (let ([called (map string->number (string-split (stream-first s) ","))]
	[boards (stream-map
		  (lambda (g) (map (lambda (l) (map string->number (string-split l))) g))
		  (stream-group (stream-rest (stream-rest s))))])
    (cons called (stream->list boards))))

(define (with-data f)
  (call-with-input-file
    "../data/day4"
    (compose f parse-data sequence->stream in-lines)))

(define (with-test-data f)
  (f (parse-data (sequence->stream (string-split #<<EOF
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
EOF
"\n")))))

(define (lines b)
  (append b (map (lambda (i) (map (lambda (l) (list-ref l i))
				  b))
		 (range (length (car b))))))

(define (done1? ls) (and (ormap null? ls) ls))

(define (step n bls)
  (map (lambda (ls) (map (lambda (l) (set-remove l n)) ls)) bls))

; Easiest to convert boards into all rows and also all columns
; this doubles the memory usage, but is easier to work with
(define/match (part1 d)
  [((cons c bs))
   (let ([done? (lambda (bls) (ormap done1? bls))])
     (let go ([n #f] [ns c] [bls (map lines bs)])
       (match (done? bls)
	 [#f (go (car ns) (cdr ns) (step (car ns) bls))]
	 [x (* n (/ (apply + (apply append x)) 2))])))])

(printf "part 1: ~a\n"
	(with-data part1))

(define/match (part2 d)
  [((cons c bs))
   (let ([done? (lambda (bls) (andmap done1? bls))]
	 [step (lambda (n bls)
		 (step n (filter (compose not done1?) bls)))])
     (let go ([n #f] [ns c] [bls (map lines bs)])
       (match (done? bls)
	 [#f (go (car ns) (cdr ns) (step (car ns) bls))]
	 [x (* n (/ (apply + (apply append x)) 2))])))])

(printf "part 2: ~a\n"
	(with-data part2))
