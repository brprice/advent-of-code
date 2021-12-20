#lang racket

; represent the tree as a flat list of tokens
(define (parse l)
 (define h (match-lambda
	      [#\[ '(L)]
	      [#\] '(R)]
	      [#\, '()]
	      [n (list (string->number (list->string (list n))))]))
 (apply append (map h (string->list l))))

(define (with-data f)
  (call-with-input-file
    "../data/day18"
    (lambda (p)
      (let* ([lines (in-lines p)]
	     [ns (sequence-map parse lines)])
	(f (sequence->list ns))))))

(define (with-test-data f)
  (let ([data '("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
	      "[[[5,[2,8]],4],[5,[[9,9],0]]]"
	      "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
	      "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
	      "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
	      "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
	      "[[[[5,4],[7,7]],8],[[8,3],8]]"
	      "[[9,3],[[9,9],[6,[4,9]]]]"
	      "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
	      "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")])
    (f (map parse data))))

; a list zipper, but records the tree path also
(define (zipper t)
  (list '() '() t))

(define (depth z)
  (length (car z)))

(define/match (right z)
	      [((list d prev '()))
	       #f]
	      [((list d prev (cons 'L xs)))
	       (list (cons 'L d) (cons 'L prev) xs)]
	      [((list (cons _ '()) prev (cons 'R xs)))
	       (list '() (cons 'R prev) xs)]
	      [((list (cons _ d) prev (cons 'R xs)))
	       (list (cons 'R (cdr d)) (cons 'R prev) xs)]
	      [((list (cons _ d) prev (cons n xs)))
	       (list (cons 'R d) (cons n prev) xs)])

(define/match (rewind-to-start z)
	      [((list _ prev next)) (list '() '() (append (reverse prev) next))])

(define (add-first n l)
  (match l
	 [(cons h t)
	  #:when (number? h)
	  (cons (+ n h) t)]
	 [(cons h t)
	  (cons h (add-first n t))]
	 ['() '()]))

; does the next (i.e. leftmost after/at the focus) explosion,
; leaving the focus on the now-zeroed exploded pair
; returns #f if no explosions possible
(define/match (explode-next z)
	      [((list d prev (list* 'L n m 'R rest)))
	       #:when (and (<= 4 (depth z)) (number? n) (number? m))
	       (list d (add-first n prev) (cons 0 (add-first m rest)))]
	      [((list _ _ '())) #f]
	      [(_) (explode-next (right z))])

; does all explosions, leaves zipper on the left
(define (explode z)
  (match (explode-next z)
	 [#f (rewind-to-start z)]
	 [z1 (explode z1)]))

; does the first split, leaving the zipper on the left
; returns #f if no splits possible
(define/match (split z)
	      [((list d prev (cons n rest)))
	       #:when (and (number? n) (<= 10 n))
	       (let-values ([(q r) (quotient/remainder n 2)])
		 (rewind-to-start (list d prev (list* 'L q (+ q r) 'R rest))))]
	      [((list _ _ '())) #f]
	      [(_) (split (right z))])

; only uses zippers internally, the input and output are both lists of tokens
; (i.e. trees)
(define (reduce t)
  (define (reducez z)
    (let ([exploded (explode z)])
      (match (split exploded)
	     [#f exploded]
	     [s (reducez s)])))
  (caddr (reducez (zipper t))))

(define/match (add ts)
	      [((list* t1 t2 rest))
	       (add (cons (reduce (append '(L) t1 t2 '(R))) rest))]
	      [((list t)) t])

(define (magnitude t)
  (define (weight ds)
    (for/product [(d ds)]
		 (if (eq? d 'L) 3 2)))
  (define/match (mag m z)
		[(m (list _ _ '())) m]
		[(m (list d _ (cons n _)))
		 #:when (number? n)
		 (mag (+ m (* (weight d) n)) (right z))]
		[(m _) (mag m (right z))])
  (mag 0 (zipper t)))

(define (part1 t)
  (magnitude (add t)))

(printf "part 1: ~a\n"
	(with-data part1))
