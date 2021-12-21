#lang racket

(define test-data '(4 8))

(define data
  (for/list ([l (in-lines (open-input-file "../data/day21"))])
	    (string->number (cadr (string-split l ": ")))))

(define (part1 data)
  (define (wrap n)
    (add1 (modulo (sub1 n) 10)))
  (define die 1)
  (define (roll-die)
    (let ([d die])
      (set! die (if (eq? d 100) 1 (add1 d)))
      d))
  ; keep the next player to move as the first elt
  (define (step ps)
    (let* ([r (+ (roll-die) (roll-die) (roll-die))]
	   [new-pos (wrap (+ (caar ps) r))])
      (list (cadr ps) (list new-pos (+ new-pos (cadar ps))))))
  (define (go rs ps)
    (let ([new-ps (step ps)]
	  [new-rs (+ 3 rs)])
      (if (<= 1000 (cadadr new-ps))
	(* (cadar new-ps) new-rs)
	(go new-rs new-ps))))
  (go 0 (list (list (car data) 0) (list (cadr data) 0))))

(printf "part 1: ~a\n"
	(part1 data))

(define (part2 data)
  (define board-size 10)
  (define win-score 21)
  (define rolls (for*/list ([i '(1 2 3)][j '(1 2 3)][k '(1 2 3)]) (+ i j k)))
  (define start (list (car data) 0 (cadr data) 0))
  (define (indices-help n)
    (let ([ss1 (map (lambda (s2) (cons n s2)) (range (add1 n)))]
	  [ss2 (map (lambda (s1) (cons s1 n)) (range n))])
      (for*/list ([ss (append ss1 ss2)]
		  [p (range 1 (add1 board-size))]
		  [q (range 1 (add1 board-size))])
		 (list p (car ss) q (cdr ss)))))
  (define indices
    (append-map indices-help (reverse (range (+ win-score board-size)))))
  (define dp
    (make-hash (map (lambda (i) (cons i #f)) indices)))
  (define (wrap n)
    (add1 (modulo (sub1 n) board-size)))
  (define (step n ps)
    (let ([new-pos (wrap (+ (car ps) n))])
      (list (caddr ps) (cadddr ps) new-pos (+ new-pos (cadr ps)))))
  (define (add . xs)
    (list (apply + (map car xs)) (apply + (map cadr xs))))
  (define (swap p) (list (cadr p) (car p)))
  (define/match (f i)
		[((list _ s _ t))
		 (hash-set! dp
			    i
			    (cond
			      [(<= win-score s) '(1 0)]
			      [(<= win-score t) '(0 1)]
			      [else (swap
				      (apply add
					     (map
					       (lambda (r) (hash-ref dp (step r i)))
					       rolls)))]))])
  (for-each f indices)
  (apply max (hash-ref dp start)))

(printf "part 2: ~a\n"
	(part2 data))
