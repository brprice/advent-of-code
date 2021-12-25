#lang racket

(define (parse s)
  (let* ([locs1 (for*/list ([(l j) (in-parallel (string-split s) (in-naturals))]
			    [(c i) (in-parallel (string->list l) (in-naturals))])
			   (match c
				  [#\> (cons 'E (list i j))]
				  [#\v (cons 'S (list i j))]
				  [#\. (cons #f (list i j))]))]
	 [mx (apply max (map cadr locs1))]
	 [my (apply max (map caddr locs1))]
	 [locs2 (filter car locs1)]
	 [herds (group-by car locs2)]
	 [es (if (eq? (caaar herds) 'E)
	       (cons (car herds) (cadr herds))
	       (cons (cadr herds) (car herds)))]
	 [e (list->set (map cdr (car es)))]
	 [s (list->set (map cdr (cdr es)))])
    (list mx my e s)))

(define test-data (parse "\
v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>"))

(define data (parse (port->string (open-input-file "../data/day25"))))

(define ((step mx my) herd-e herd-s)
  (define (step-e c)
    (let* ([x1 (add1 (car c))]
	   [x2 (if (< mx x1) 0 x1)])
      (list x2 (cadr c))))
  (define (step-s c)
    (let* ([y1 (add1 (cadr c))]
	   [y2 (if (< my y1) 0 y1)])
      (list (car c) y2)))
  (define (step1 f to-step others)
    (for/fold ([new-herd '()] [any-moved #f] #:result (cons (apply set new-herd) any-moved))
	      ([c to-step])
	      (let ([c-new (f c)])
		(if (or (set-member? to-step c-new) (set-member? others c-new))
		  (values (cons c new-herd) any-moved)
		  (values (cons c-new new-herd) #t)))))
  (let* ([e-new--e-moved (step1 step-e herd-e herd-s)]
	 [e-new (car e-new--e-moved)]
	 [e-moved (cdr e-new--e-moved)]
	 [s-new--s-moved (step1 step-s herd-s e-new)]
	 [s-new (car s-new--s-moved)]
	 [s-moved (cdr s-new--s-moved)])
  (cons (or e-moved s-moved) (list e-new s-new))))

(define/match (part1 data)
	      [((list* mx my herds))
	       (letrec ([f (step mx my)]
			[loop (lambda (n herds)
				(match (apply f herds)
				       [(cons #t herds-new) (loop (add1 n) herds-new)]
				       [(cons #f _) n]))])
		 ; start with 1 as loop counts number of steps where something moves,
		 ; and we want first step with no movement
		 (loop 1 herds))])

(printf "part 1: ~a\n"
	(part1 data))

; NB: there was only one part today!
