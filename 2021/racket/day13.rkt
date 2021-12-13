#lang racket

(define-syntax-rule (push! x xs)
  (set! xs (cons x xs)))

(define (with-data f)
  (call-with-input-file
    "../data/day13"
    (lambda (p)
      (let ([pts (mutable-set)]
	    [folds '()])
	(do ([l (read-line p) (read-line p)])
	  ((string=? "" l))
	  (match (string-split l ",")
		 [(list x y) (set-add! pts
				       (list (string->number x)
					     (string->number y)))]))
	(do ([l (read-line p) (read-line p)])
	  ((eof-object? l))
	  (match (string-split (caddr (string-split l)) "=")
		 [(list dir pos) (push! (list (string->symbol dir) (string->number pos)) folds)]))
	(f (list pts (reverse folds)))))))

(define (with-test-data f)
  (let ([pts '((6 10)
	       (0 14)
	       (9 10)
	       (0 3)
	       (10 4)
	       (4 11)
	       (6 0)
	       (6 12)
	       (4 1)
	       (0 13)
	       (10 12)
	       (3 4)
	       (3 0)
	       (8 4)
	       (1 10)
	       (2 14)
	       (8 10)
	       (9 0))])
    (f (list (list->mutable-set pts) '((y 7) (x 5))))))

(define (fold! f xs)
  (let* ([l (cadr f)]
	 [k (if (eq? 'x (car f)) car cadr)]
	 [flip (if (eq? 'x (car f))
		 (lambda (p) (list (- (* 2 l) (car p)) (cadr p)))
		 (lambda (p) (list (car p) (- (* 2 l) (cadr p)))))])
    (set-for-each xs
		  (lambda (p) (when (> (k p) l)
				(set-remove! xs p)
				(set-add! xs (flip p)))))))


(define (part1 xs)
  (match xs
	 [(list pts (cons f1 _))
	  (fold! f1 pts)
	  (set-count pts)]))

(printf "part 1: ~a\n"
	(with-data part1))
