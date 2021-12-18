#lang racket

(require data/queue)

(define (with-data f)
  (call-with-input-file
    "../data/day15"
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
  (let ([data '(1 1 6 3 7 5 1 7 4 2
		1 3 8 1 3 7 3 6 7 2
		2 1 3 6 5 1 1 3 2 8
		3 6 9 4 9 3 1 5 6 9
		7 4 6 3 4 1 7 1 1 1
		1 3 1 9 1 2 8 1 3 7
		1 3 5 9 9 1 2 4 2 1
		3 1 2 5 4 2 1 6 3 9
		1 2 9 3 1 3 8 5 2 1
		2 3 1 1 9 4 4 5 8 1)])
    (f (vec2 10 10 (apply vector data)))))

(define (vec2 x y v)
  (list v x y (lambda (i j) (+ i (* x j)))))

(define (vec2-width xs) (cadr xs))
(define (vec2-height xs) (caddr xs))
(define (vec2-lookup xs i j) (vector-ref (car xs) ((cadddr xs) i j)))
(define (vec2-set! xs i j v) (vector-set! (car xs) ((cadddr xs) i j) v))
(define (vec2-data xs) (car xs))

(define (build-vec2 w h f)
  (vec2 w h
	(build-vector (* w h)
		      (lambda (ij)
			(call-with-values
			  (thunk (quotient/remainder ij w))
			  (lambda (j i) (f i j)))))))


(define (adjacent-idx xs i j)
  (filter (lambda (a) (and (< -1 (car a) (vec2-width xs))
                           (< -1 (cadr a) (vec2-height xs))))
          (list (list (sub1 i) j)
                (list (add1 i) j)
                (list i (sub1 j))
                (list i (add1 j)))))

(define (shortest-paths xs start-i start-j)
  (let* ([searching (make-queue)]
	 [w (vec2-width xs)]
	 [h (vec2-height xs)]
	 [best (vec2 w h (make-vector (* (vec2-width xs) (vec2-height xs)) -1))])
    (enqueue! searching (list start-i start-j))
    (vec2-set! best start-i start-j 0)
    (do ()
      ((queue-empty? searching) best)
      (let* ([p (dequeue! searching)]
	     [c (apply vec2-lookup best p)])
	(for* ([adj (apply adjacent-idx xs p)]
	       [adjC (list (apply vec2-lookup xs adj))]
	       [totC (list (+ c adjC))]
	       [adjBest (list (apply vec2-lookup best adj))])
	      (when (or (eq? -1 adjBest ) (< totC adjBest))
	       (vec2-set! best (car adj) (cadr adj) totC)
	       (enqueue! searching adj)))))))

(define (part1 xs)
  (vec2-lookup (shortest-paths xs 0 0)
	       (sub1 (vec2-width xs))
	       (sub1 (vec2-height xs))))

(printf "part 1: ~a\n"
	(with-data part1))

; part 2: let's not be smart, just work on the 5^2-fold larger graph
(define (part2 xs)
  (let* ([xw (vec2-width xs)]
         [xh (vec2-height xs)]
	 [ys (build-vec2 (* 5 xw) (* 5 xh)
			 (lambda (i j)
			   (let-values ([(iq ir) (quotient/remainder i xw)]
			                [(jq jr) (quotient/remainder j xh)])
			     ; urgh, wraps like 8,9,1,2 not 8,9,0,1
			     ;(remainder (+ iq jq (vec2-lookup xs ir jr)) 10)
			     (add1 (remainder (+ -1 iq jq (vec2-lookup xs ir jr)) 9)))))])
    (vec2-lookup (shortest-paths ys 0 0)
		 (sub1 (vec2-width ys))
		 (sub1 (vec2-height ys)))))

(printf "part 2: ~a\n"
	(with-data part2))
