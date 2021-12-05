#lang racket

; Convert input data to a pair of pairs:
; - car: start point, so caar: x1, cdar: y1
; - cdr: end point, so cadr: x2, cddr: y2
(define (parse-data s)
  (define (parsexy s)
    (match (string-split s ",")
	 [(list x y) (cons (string->number x) (string->number y))]))
  (match (string-split s " -> ")
	 [(list start end) (cons (parsexy start) (parsexy end))]))

(define (with-data f)
  (call-with-input-file
    "../data/day5"
    (compose f (curry stream-map parse-data) sequence->stream in-lines)))

(define (with-test-data f)
  (f ((curry stream-map parse-data) (sequence->stream (string-split #<<EOF
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
EOF
"\n")))))

(define/match (ortho? l)
  [((cons (cons x1 y1) (cons x2 y2)))
   (or (eq? x1 x2) (eq? y1 y2))])

; NB: only for orthogonal lines - I expect to have to change this in part 2
(define/match (points l)
  [((cons (cons x1 y1) (cons x2 y2)))
   (cond
     [(eq? x1 x2) (map (lambda (y) (cons x1 y)) (range (min y1 y2) (add1 (max y1 y2))))]
     [(eq? y1 y2) (map (lambda (x) (cons x y1)) (range (min x1 x2) (add1 (max x1 x2))))])])

; Easiest (but not very efficient) to convert vent line data into sets of points
(define (part1 vls)
  (let* ([covers (make-hash)]
	 [ins (lambda (ps) (for-each (lambda (p) (dict-update! covers p add1 0)) ps))])
    (stream-for-each (compose ins points) (stream-filter ortho? vls))
    (length (filter (lambda (c) (< 1 c)) (hash-values covers)))))

(printf "part 1: ~a\n"
	(with-data part1))
