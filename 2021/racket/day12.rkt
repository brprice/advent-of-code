#lang racket

(define (string->numbers s)
  (map (lambda (c) (string->number (list->string (list c)))) (string->list s)))

(define (with-data f)
  (call-with-input-file
    "../data/day12"
    (lambda (p)
      (let ([g (lambda (l)
		 (match (string-split l "-")
			[(list a b) (cons (string->symbol a) (string->symbol b))]))
			])
	(f (map g (sequence->list (in-lines p))))))))

(define (with-test-data f)
  (f '((start . A)
       (start . b)
       (A . c)
       (A . b)
       (b . d)
       (A . end)
       (b . end))))

(define-syntax-rule (pop! xs)
  (set! xs (cdr xs)))

(define-syntax-rule (push! x xs)
  (set! xs (cons x xs)))

; simple-minded brute force, actually finding all paths
(define (paths g)
  (let*
    ([big-cave (lambda (n) (eq? n (string->symbol (string-upcase (symbol->string n)))))]
     [can-move (lambda (p n) (or (big-cave n) (not (member n p))))]
     [nshelp (lambda (x) (lambda (e) (if (eq? x (car e)) (cdr e) (car e))))]
     [ns (lambda (x)
	   (map (nshelp x)
		(filter (lambda (e) (or (eq? (car e) x) (eq? (cdr e) x)))
			g)))]
     [searching '((start))]
     [paths '()])
    (do ()
      ((null? searching) paths)
      (let ([p (car searching)])
	(pop! searching)
	(if (eq? 'end (car p))
	  (push! p paths)
	  (for ([n (ns (car p))])
	       (when (can-move p n)
		 (push! (cons n p) searching))))))))

(define (part1 xs)
  (length (paths xs)))

(printf "part 1: ~a\n"
	(with-data part1))
