#lang racket

; data format: '(step-table default non-default-idxs)
; i.e. 1st is the lookup table that gives the evolution rule
; 2nd is the value of cofinitely many cells
; 3rd are the finitely many cells with a different value
(define test-data
  '(#(0 0 1 0 1 0 0 1 1 1 1 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 0 0 0 0 1 1 1 0 1 1 0 1 0 0 1 1 1 0 1 1 1 1 0 0 1 1 1 1 1 0 0 1 0 0 0 0 1 0 0 1 0 0 1 1 0 0 1 1
      1 0 0 1 1 1 1 1 1 0 1 1 1 0 0 0 1 1 1 1 0 0 1 0 0 1 1 1 1 1 0 0 1 1 0 0 1 0 1 1 1 1 1 0 0 0 1 1 0 1 0 1 0 0 1 0 1 1 0 0 1 0 1 0 0 0 0 0 0 1 0 1 1 1
      0 1 1 1 1 1 1 0 1 1 1 0 1 1 1 1 0 0 0 1 0 1 1 0 1 1 0 0 1 0 0 1 0 0 1 1 1 1 1 0 0 0 0 0 1 0 1 0 0 0 0 1 1 1 0 0 1 0 1 1 0 0 0 0 0 0 1 0 0 0 0 0 1 0
      0 1 0 0 1 0 0 1 1 0 0 1 0 0 0 1 1 0 1 1 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 1 0 1 0 1 0 0 0 1 1 1 1 0 1 1 0 1 0 0 0 0 0
      0 1 0 0 1 0 0 0 1 1 0 1 0 1 1 0 0 1 0 0 0 1 1 0 1 0 1 1 0 0 1 1 1 0 1 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 1 0 1 0 1 0 1 1 1 1 0 1 1 1 0 1 1 0 0 0 1 0 0
      0 0 0 1 1 1 1 0 1 0 0 1 0 0 1 0 1 1 0 1 0 0 0 0 1 1 0 0 1 0 1 1 1 1 0 0 0 0 1 1 0 0 0 1 1 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0
      0 0 1 1 0 0 1 1 1 1 0 0 1 0 0 0 1 0 1 0 1 0 0 0 1 1 0 0 1 0 1 0 0 1 1 1 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 1 0 0 1 1 1 1 0 0 0 0 0 0 1 0 0 1)
    0
    ((0 0) (3 0) (0 1) (0 2) (1 2) (4 2) (2 3) (2 4) (3 4) (4 4))))

(define data
  (let* ([ls (in-lines (open-input-file "../data/day20"))]
	 [step-dat (sequence-ref ls 0)]
	 [parse-char (lambda (c) (if (eq? c #\#) 1 0))]
	 [step (apply vector (map parse-char (string->list step-dat)))]
	 [_ (sequence-ref ls 0)] ; skip blank line
	 [lit '()])
    (for ([l ls] [y (in-naturals)])
	 (for ([c (string->list l)] [x (in-naturals)])
	      (when (eq? c #\#) (set! lit (cons (list x y) lit)))))
    (list step 0 (list->set lit))))

; Returns nbd indices in most-significant-bit-first order
(define (nbdhood-idxs p)
  (for*/list ([j '(-1 0 1)] [i '(-1 0 1)])
	     (list (+ i (car p)) (+ j (cadr p)))))

(define (lookup-pixel img p)
  (if (set-member? (caddr img) p)
    (- 1 (cadr img))
    (cadr img)))

(define (bits->number bs)
  (foldl (lambda (b acc) (+ (* 2 acc) b)) 0 bs))

(define (enhance1 img p)
  (vector-ref (car img) (bits->number (map (curry lookup-pixel img) (nbdhood-idxs p)))))

(define/match (enhance img)
  [((list step most idxs))
   (let ([most-new (vector-ref step (if (eq? most 0) 0 511))]
	 [consider (list->set (append-map nbdhood-idxs (set->list idxs)))]
	 [idxs-new '()])
     (set-for-each consider
		   (lambda (p)
		     (unless (eq? (enhance1 img p) most-new)
		       (set! idxs-new (cons p idxs-new)))))
     (list step most-new (list->set idxs-new)))])

(define (part1 data)
  (set-count (caddr (enhance (enhance data)))))

(printf "part 1: ~a\n"
	(part1 data))

(define (part2 data)
  (set-count (caddr ((apply compose (make-list 50 enhance)) data))))

(printf "part 2: ~a\n"
	(part2 data))
