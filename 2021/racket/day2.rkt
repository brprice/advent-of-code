#lang racket

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
		(cons cmd (string->number arg))))
	    (in-lines p)))))))

(define (with-test-data f)
  (f (list
       (cons "forward" 5)
       (cons "down" 5)
       (cons "forward" 8)
       (cons "up" 3)
       (cons "down" 8)
       (cons "forward" 2))))

; part 1: we will operate on pairs of horizontal position and depth
(define (forward1 n p) (cons (+ (car p) n) (cdr p)))
(define (up1 n p) (cons (car p) (- (cdr p) n)))
(define (down1 n p) (cons (car p) (+ (cdr p) n)))

(define (get-cmd1 cmd)
  (match cmd
	 ["forward" forward1]
	 ["up" up1]
	 ["down" down1]))

(define (part1 s)
  (let* ([f (lambda (acc cmd-arg) ((get-cmd1 (car cmd-arg)) (cdr cmd-arg) acc))]
	 [target (sequence-fold f (cons 0 0) s)])
  (* (car target) (cdr target))))

(printf "part 1: ~a\n"
	(with-data part1))

; part 2: we will operate on lists of horizontal position, depth and aim
(define (forward2 n p) (list (+ (car p) n) (+ (cadr p) (* (caddr p) n)) (caddr p)))
(define (up2 n p) (list (car p) (cadr p) (- (caddr p) n)))
(define (down2 n p) (list (car p) (cadr p) (+ (caddr p) n)))

(define (get-cmd2 cmd)
  (match cmd
	 ["forward" forward2]
	 ["up" up2]
	 ["down" down2]))

(define (part2 s)
  (let* ([f (lambda (acc cmd-arg) ((get-cmd2 (car cmd-arg)) (cdr cmd-arg) acc))]
	 [target (sequence-fold f (list 0 0 0) s)])
  (* (car target) (cadr target))))

(printf "part 2: ~a\n"
	(with-data part2))
