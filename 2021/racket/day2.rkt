#lang racket

(define (get-cmd cmd)
  (match cmd
	 ["forward" forward]
	 ["up" up]
	 ["down" down]))

(define (with-data f)
  (call-with-input-file
    "../data/day2"
    (lambda (p)
      (f
	(sequence->stream
	  (sequence-map
	    (lambda (s)
	      (let* ([words (string-split s)]
		     [cmd (car words)]
		     [arg (cadr words)])
		(cons (get-cmd cmd) (string->number arg))))
	    (in-lines p)))))))

(define (with-test-data f)
  (f (list
       (cons forward 5)
       (cons down 5)
       (cons forward 8)
       (cons up 3)
       (cons down 8)
       (cons forward 2))))

; we will operate on pairs of horizontal position and depth
(define (forward n p) (cons (+ (car p) n) (cdr p)))
(define (up n p) (cons (car p) (- (cdr p) n)))
(define (down n p) (cons (car p) (+ (cdr p) n)))

(define (part1 s)
  (let* ([f (lambda (acc cmd-arg) ((car cmd-arg) (cdr cmd-arg) acc))]
	 [target (sequence-fold f (cons 0 0) s)])
  (* (car target) (cdr target))))

(printf "part 1: ~a\n"
	(with-data part1))
