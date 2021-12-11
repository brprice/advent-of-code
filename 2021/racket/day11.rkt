#lang racket

(define (string->numbers s)
  (map (lambda (c) (string->number (list->string (list c)))) (string->list s)))

(define (with-data f)
  (call-with-input-file
    "../data/day11"
    (let ([g (lambda (l) (map (compose string->number list->string list)
			      (string->list l)))])
      (lambda (p)
	(let* ([ls (in-lines p)]
	       [l1 (g (sequence-ref ls 0))]
	       [w (length l1)]
	       [res (sequence-fold (lambda (acc l) (cons (add1 (car acc)) (append (cdr acc) (g l))))
			 (cons 1 l1) ls)]
	       [h (car res)]
	       [v (list->vector (cdr res))])
	(f (vec2 w h v)))))))

(define (with-test-data f)
  (let ([data '(5 4 8 3 1 4 3 2 2 3
		2 7 4 5 8 5 4 7 1 1
		5 2 6 4 5 5 6 1 7 3
		6 1 4 1 3 3 6 1 4 6
		6 3 5 7 3 8 5 4 7 8
		4 1 6 7 5 2 4 6 4 5
		2 1 7 6 8 4 1 7 2 1
		6 8 8 2 8 8 1 1 3 4
		4 8 4 6 8 4 8 5 5 4
		5 2 8 3 7 5 1 5 2 6)])
    (f (vec2 10 10 (apply vector data)))))

(define (vec2 x y v)
  (list v x y (lambda (i j) (+ i (* x j)))))

(define (vec2-width xs) (cadr xs))
(define (vec2-height xs) (caddr xs))
(define (vec2-lookup xs i j) (vector-ref (car xs) ((cadddr xs) i j)))
(define (vec2-set! xs i j v) (vector-set! (car xs) ((cadddr xs) i j) v))

(define (adjacent-idx v2 i j)
  (for*/list ([u '(-1 0 1)]
	      [v '(-1 0 1)]
	      #:unless (and (eq? u 0) (eq? v 0))
	      [iu (list (+ i u))]
	      [jv (list (+ j v))]
	      #:unless (> 0 iu)
	      #:unless (> 0 jv)
	      #:unless (>= iu (vec2-width v2))
	      #:unless (>= jv (vec2-height v2)))
	     (list iu jv)))

(define (step v2)
  (let* ([flashes 0]
	 [sparks '()]
	 [inc (lambda (i j)
		(let ([x (vec2-lookup v2 i j)])
		  (if (<= 9 x)
		    (begin (set! sparks (cons (list i j) sparks))
			   (vec2-set! v2 i j -100))
		    (vec2-set! v2 i j (add1 x)))))])
    (for* ([i (vec2-width v2)]
	   [j (vec2-height v2)])
	  (inc i j))
    (do ()
      ((null? sparks))
      (let ([ij (car sparks)])
	(set! flashes (add1 flashes))
	(set! sparks (cdr sparks))
	(map (curry apply inc) (apply adjacent-idx v2 ij))))
    (for* ([i (vec2-width v2)]
	   [j (vec2-height v2)]
	   [x (list (vec2-lookup v2 i j))])
	  (when (> 0 x) (vec2-set! v2 i j 0)))
    (cons flashes v2)))

(define (part1 xs)
  (let ([flashes 0])
    (do ([i 0 (add1 i)])
      ((eq? i 100) flashes)
      (set! flashes (+ flashes (car (step xs)))))))

(printf "part 1: ~a\n"
	(with-data part1))
