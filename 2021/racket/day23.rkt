#lang racket

(require data/heap)

; I assume the burrow is of the form of example-burrow
; except for a permutation of the amphipods

(define example-burrow
"\
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########")

(define target-burrow
"\
#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########")

(define (parse b)
  (match (string-split b "\n")
	 [(list "#############"
		"#...........#"
		ru
		rl
		"  #########")
	  (match (regexp-split #rx"#" ru)
		 [(list "" "" "" ru1 ru2 ru3 ru4 "" "" "")
		  (match (regexp-split #rx"#" rl)
			 [(list "  " rl1 rl2 rl3 rl4 "")
			  (burrow (list ru1 ru2 ru3 ru4) (list rl1 rl2 rl3 rl4))
			  ]
			 [_ (raise-argument-error 'parse
						  "4th line of form \"  #.#.#.#.#\""
						  rl)])]
		 [_ (raise-argument-error 'parse
					  "3rd line of form \"###.#.#.#.###\""
					  rl)])]
	 [_ (raise-argument-error 'parse
				  (format "input of same form as ~s" example-burrow)
				  b)]))

(define (mkhash kvs)
  (apply hash
	 (append-map (lambda (p) (list (car p) (cdr p))) kvs)))

(define (burrow u l)
  (mkhash (append (map (lambda (a x) (cons (list x 2) a)) u '(3 5 7 9))
		  (map (lambda (a x) (cons (list x 3) a)) l '(3 5 7 9)))))

(define test-data
  (parse example-burrow))

(define data
  (parse (port->string (open-input-file "../data/day23"))))

(define/match (neighbours room-depth p)
	      [(_ (list x 1))
	       (filter
		 (compose not null?)
		 (list
		   (if (eq? x 1)
		     '()
		     (list (sub1 x) 1))
		   (if (eq? x 11)
		     '()
		     (list (add1 x) 1))
		   (if (and (< 1 x 11) (odd? x))
		     (list x 2)
		     '())))]
	      [(_ (list x d))
	       (filter (lambda (q) (<= 1 (cadr q) room-depth))
		       (list (list x (sub1 d)) (list x (add1 d))))])

(define (occupied b p)
  (dict-has-key? b p))

(define (reachable room-depth b p)
  (define (loop rch todo)
    (match todo
	   ['() rch]
	   [(cons p rest)
	    (if (or (set-member? rch p) (occupied b p))
	      (loop rch rest)
	      (loop (set-add rch p) (append (neighbours room-depth p) rest)))]))
  (set-remove (loop '() (list p)) p))

(define (cost a p q)
  (define/match (base a)
		[("A") 1]
		[("B") 10]
		[("C") 100]
		[("D") 1000])
  (define (dist p q)
    (if (eq? (car p) (car q))
      (abs (- (cadr p) (cadr q)))
      (+ (- (cadr p) 1)
	 (abs (- (car p) (car q)))
	 (- (cadr q) 1))))
  (* (base a) (dist p q)))

(define/match (correct-room a)
	      [("A") 3] [("B") 5] [("C") 7] [("D") 9])

(define (moves room-depth b p)
  (define (outside-room q)
    (and (eq? (cadr q) 1)
	 (member (car q) '(3 5 7 9))))
  (define (own-room a q)
    (let ([r (correct-room a)])
      (if (not (eq? (cadr q) 1))
	(and (eq? (car q) r)
	     (for/and ([d (range 2 (add1 room-depth))])
		      (equal? a (dict-ref b (list r d) a))))
	#t)))
  (define (hall-room q)
    (if (eq? (cadr p) 1)
      (not (eq? (cadr q) 1))
      #t))
  (define (stupid-move a q)
    (or
      (and (eq? (car p) (correct-room a))
	   (for/and ([d (range (add1 (cadr p)) (add1 room-depth))])
		    (equal? (dict-ref b (list (correct-room a) d) #f) a)))
      (and (< 1 (cadr q) room-depth)
	   (not (occupied b (list (car q) (add1 (cadr q))))))))
  (define (ok-move a q)
    (and (not (outside-room q))
	 (own-room a q)
	 (hall-room q)
	 (not (stupid-move a q))))
  (let ([a (dict-ref b p)]
	[b-a (dict-remove b p)])
    (apply append
	   (for/list ([r (reachable room-depth b-a p)])
		     (if (ok-move a r)
		       (list (cons (cost a p r) (dict-set b-a r a)))
		       '())))))

(define (deadlock p)
  (let ([corridor (filter (lambda (qa) (eq? 1 (cadar qa))) (dict->list p))])
    (for*/or ([u corridor] [v corridor] #:when (< (caar u) (caar v)))
	     (and (< (correct-room (cdr v)) (caar u))
		  (< (caar v) (correct-room (cdr u)))))))

(define (all-moves room-depth b)
  (filter (compose not deadlock cdr)
	  (apply append (dict-map b (lambda (p _) (moves room-depth b p))))))

(define (all-moves-1 b)
  (all-moves 3 b))

(define (search init moves heur tgt)
  (define (cmp x y) (<= (+ (car x) (cadr x)) (+ (car y) (cadr y))))
  (define heap (make-heap cmp))
  (define (go seen)
    (if (eq? 0 (heap-count heap))
      (error "exhausted the search space")
      (match (heap-min heap)
	     [(list cost _ pos)
	      (if (set-member? seen pos)
		(begin (heap-remove-min! heap) (go seen))
		(if (tgt pos)
		  (list cost pos)
		  (begin
		    (heap-remove-min! heap)
		    (for-each (lambda (q) (heap-add! heap (list (+ cost (car q))
								(heur (cdr q))
								(cdr q))))
			      (moves pos))
		    (go (set-add seen pos)))))])))
  (heap-add! heap (list 0 (heur init) init))
  (go (set)))

(define target-data
  (parse target-burrow))

(define (heuristic p)
  (for/sum ([(q a) (in-dict p)])
	   (match q
		  [(list x y)
		   (if (eq? (correct-room a) x)
		     0
		     (cost a (list x y) (list (correct-room a) 1)))])))

(define (part1 data)
  (car (search data
	       all-moves-1
	       heuristic
	       (curry equal? target-data))))

(printf "part 1: ~a\n"
	(part1 data))

(define (all-moves-2 b)
  (all-moves 5 b))

(define (part2 data)
  (define extra-init '(((3 3) . "D") ((3 4) . "D")
		       ((5 3) . "C") ((5 4) . "B")
		       ((7 3) . "B") ((7 4) . "A")
		       ((9 3) . "A") ((9 4) . "C")))
  (define extra-targ '(((3 3) . "A") ((3 4) . "A")
		       ((5 3) . "B") ((5 4) . "B")
		       ((7 3) . "C") ((7 4) . "C")
		       ((9 3) . "D") ((9 4) . "D")))
  (define (add-extra extra data)
    (mkhash (append extra
		    (dict-map data
			      (lambda (k v) (cons (list (car k)
							(if (eq? (cadr k) 3) 5 (cadr k)))
						  v))))))
  (car (search (add-extra extra-init data)
	       all-moves-2
	       heuristic
	       (curry equal? (add-extra extra-targ target-data)))))

(printf "part 2: ~a\n"
	(part2 data))
