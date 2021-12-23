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

(define (burrow u l)
  (apply hash
	 (append-map (lambda (p) (list (car p) (cdr p)))
		     (append
		       (map (lambda (a x) (cons (list x 2) a)) u '(3 5 7 9))
		       (map (lambda (a x) (cons (list x 3) a)) l '(3 5 7 9))))))

(define test-data
  (parse example-burrow))

(define data
  (parse (port->string (open-input-file "../data/day23"))))

(define/match (neighbours p)
	      [((list x 1))
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
	      [((list x 2)) (list (list x 1) (list x 3))]
	      [((list x 3)) (list (list x 2))])

(define (occupied b p)
  (dict-has-key? b p))

(define (reachable b p)
  (define (loop rch todo)
    (match todo
	   ['() rch]
	   [(cons p rest)
	    (if (or (set-member? rch p) (occupied b p))
	      (loop rch rest)
	      (loop (set-add rch p) (append (neighbours p) rest)))]))
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

(define (moves b p)
  (define (outside-room q)
    (and (eq? (cadr q) 1)
	 (member (car q) '(3 5 7 9))))
  (define (own-room a q)
    (let ([r (correct-room a)])
      (if (not (eq? (cadr q) 1))
	(and (eq? (car q) r)
	     (equal? a (dict-ref b (list r 2) a))
	     (equal? a (dict-ref b (list r 3) a)))
	#t)))
  (define (hall-room q)
    (if (eq? (cadr p) 1)
      (not (eq? (cadr q) 1))
      #t))
  (define (stupid-move a q)
    (or (equal? p (list (correct-room a) 3))
	(and (equal? p (list (correct-room a) 2))
	     (equal? (dict-ref b (list (correct-room a) 3) #f) a))
	(and (eq? 2 (cadr q))
	     (not (occupied b (list (car q) 3))))))
  (define (ok-move a q)
    (and (not (outside-room q))
	 (own-room a q)
	 (hall-room q)
	 (not (stupid-move a q))))
  (let ([a (dict-ref b p)]
	[b-a (dict-remove b p)])
    (apply append
	   (for/list ([r (reachable b-a p)])
		     (if (ok-move a r)
		       (list (cons (cost a p r) (dict-set b-a r a)))
		       '())))))

(define (deadlock p)
  (let ([corridor (filter (lambda (qa) (eq? 1 (cadar qa))) (dict->list p))])
    (for*/or ([u corridor] [v corridor] #:when (< (caar u) (caar v)))
	     (and (< (correct-room (cdr v)) (caar u))
		  (< (caar v) (correct-room (cdr u)))))))

(define (all-moves b)
  (filter (compose not deadlock cdr)
	  (apply append (dict-map b (lambda (p _) (moves b p))))))

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

(define (part1 data)
  (car (search data all-moves
	       (lambda (p) (for/sum ([(q a) (in-dict p)])
				    (match q
					   [(list x y)
					    (if (eq? (correct-room a) x)
					      0
					      (cost a (list x y) (list (correct-room a) 1)))])))
	       (curry equal? target-data))))

(printf "part 1: ~a\n"
	(part1 data))
