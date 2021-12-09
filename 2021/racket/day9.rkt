#lang racket

(define (with-data f)
  (call-with-input-file
    "../data/day9"
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

(define (vec2 x y v)
  (list x y (lambda (i j)
	      (vector-ref v (+ i (* x j))))))

(define (vec2-width xs) (car xs))
(define (vec2-height xs) (cadr xs))
(define (vec2-lookup xs i j) ((caddr xs) i j))

(define (with-test-data f)
  (let ([data #(2 1 9 9 9 4 3 2 1 0
		3 9 8 7 8 9 4 9 2 1
		9 8 5 6 7 8 9 8 9 2
		8 7 6 7 8 9 6 7 8 9
		9 8 9 9 9 6 5 6 7 8)]
	[x 10]
	[y 5])
    (f (vec2 x y data))))

(define (adjacent-idx xs i j)
  (filter (lambda (a) (and (< -1 (car a) (vec2-width xs))
			   (< -1 (cadr a) (vec2-height xs))))
	  (list (list (sub1 i) j)
		(list (add1 i) j)
		(list i (sub1 j))
		(list i (add1 j)))))


(define (local-minimum xs i j)
  (let ([adj (adjacent-idx xs i j)]
	[here (vec2-lookup xs i j)])
    (andmap (lambda (a)
	      (< here (apply vec2-lookup xs a)))
	    adj)))

(define (lowpoints xs)
  (for*/list ([j (vec2-height xs)]
	     [i (vec2-width xs)]
	     #:when (local-minimum xs i j))
	    (list i j)))

(define (part1 xs)
  (for/sum ([ij (lowpoints xs)])
	   (add1 (vec2-lookup xs (car ij) (cadr ij)))))

(printf "part 1: ~a\n"
	(with-data part1))

(define-syntax-rule (define/pm (f p ...) b)
  (define/match (f _)
		[(p) ... b]))


(define (find-basin xs i j)
  (define/match (go q seen)
		[((cons ij q) seen) #:when (member ij seen) (go q seen)]
		[((cons (list i j) q) seen)
		 ;(printf "\n~a ~a" i j)
		 ;(printf " | ~a" (adjacent-idx xs i j))
		 ;(printf " | ~a" q)
		 ;(printf " | ~a" (append (adjacent-idx xs i j) q))
		 (go (append (filter (lambda (xy) (not (eq? 9 (apply vec2-lookup xs xy)))) (adjacent-idx xs i j)) q) (set-add seen (list i j)))
		 ]
		[((list) seen) seen])
  (go (list (list i j)) '()))

(define (part2 xs)
  (apply * (take
	     (sort (map (compose length
				 (curry apply find-basin xs))
			(lowpoints xs))
		   >)
	     3)))

(printf "part 2: ~a\n"
	(with-data part2))
