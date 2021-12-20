#lang racket

(define (with-data f)
  (call-with-input-file
    "../data/day17"
    (lambda (p)
      (let* ([lines (in-lines p)]
             [l (sequence-ref lines 0)]
             [s1 (cadr (string-split l "x="))]
             [s2 (string-split s1 ",")]
             [xs (string-split (car s2) "..")]
             [x (map string->number xs)]
             [s3 (string-split (cadr s2) "y=")]
             [ys (string-split (cadr s3) "..")]
             [y (map string->number ys)])
	(f x y)))))

(define (with-test-data f)
  (f '(20 30) '(-10 -5)))

; We do a bit of math to bound the problem, then brute force it.
;
; Initial x velocity has to be non-negitive, trivially,
; (we could do better, but there is no need)
; and cannot be so high the projectile passes entirely over the target in the
; first time step.
;
; One can show that y_n = n*V - n*(n-1)/2, for initial y velocity V.
; For this to land in the target y_min..y_max after n steps, we need
; y_min/n+(n-1)/2 < V < y_max/n+(n-1)/2. Since V is an integer, we deduce that
; n < 2*|y_min| is necessary
;
; Given x and n, the best we can do is V being the greatest integer in the
; above range, if any. However, we also need to ensure the x position after n
; steps is in the target area also.
;
; The highest y position is then on step n=V, when the y velocity is 0,
; i.e. y_V = V*(V + 1)/2

; returns a list of (vel_x vel_y n), where after n steps starting at said
; velocity, we hit the target
(define (all-trajectories xrange yrange)
  (let* ([xmin (apply min xrange)]
	 [xmax (apply max xrange)]
         [ymin (apply min yrange)]
         [ymax (apply max yrange)]
	 [vxmax xmax]
	 [nmax (* -2 ymin)]
	 [t (lambda (i) (quotient (* i (add1 i)) 2))]
	 [xpos (lambda (vx n) (- (t vx) (if (> n vx) 0 (t (- vx n)))))]
	 [traj '()])
    (for* ([x (add1 vxmax)] [n (add1 nmax)] #:unless (eq? 0 n) #:when (<= xmin (xpos x n) xmax))
	  (let* ([l (+ (/ ymin n) (/ (sub1 n) 2))]
		 [h (+ (/ ymax n) (/ (sub1 n) 2))])
	    (for ([v (range (ceiling l) (add1 (floor h)))])
		 (when (<= l v) (set! traj (cons (list x v n) traj))))))
    traj))

(define (part1 tgtx tgty)
  (define (height v) (if (< 0 v) (quotient (* v (add1 v)) 2) 0))
  (height (apply max (map cadr (all-trajectories tgtx tgty)))))

(printf "part 1: ~a\n"
	(with-data part1))

(define (part2 tgtx tgty)
  (length
    (remove-duplicates
      (map
	(lambda (t) (list (car t) (cadr t)))
	(all-trajectories tgtx tgty)))))

(printf "part 2: ~a\n"
	(with-data part2))
