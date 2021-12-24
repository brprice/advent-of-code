#lang racket

(define data
  (for/list ([l (in-lines (open-input-file "../data/day24"))])
	    (match (string-split l " ")
		   [(list "inp" r) (list 'inp (string->symbol r))]
		   [(list i r ri) (list (string->symbol i)
					(string->symbol r)
					(or (string->number ri) (string->symbol ri))
					)])))

; We compile the 14 blocks, each corresponding to one input datum
(define (compile prog)
  (define (is-input e)
    (and (symbol? e) (string-prefix? (symbol->string e) "D")))
  (define (pass1help n i)
    (match i
	   [(list 'inp a) (string->symbol (string-append "D" (number->string n)))]
	   [(list 'add a b) (list '+ a b)]
	   [(list 'mul a b) (list '* a b)]
	   [(list 'div a b) (list '/ a b)]
	   [(list 'mod a b) (list '% a b)]
	   [(list 'eql a b) (list 'ifeq a b 1 0)]))
  ; convert to our AST, and convert inputs to reading special vars Di
  (define (pass1 n prog)
    (match prog
	   ['() '()]
	   [(cons (list 'inp r) rest) (cons (list r (pass1help n (list 'inp r))) (pass1 (add1 n) rest))]
	   [(cons i rest) (cons (list (cadr i) (pass1help n i)) (pass1 n rest))]))
  (define/match (get-op-2 o) [('+) +] [('*) *] [('/) quotient] [('%) modulo])
  (define/match (const-fold i)
		[((list op n m)) #:when (and (number? n) (number? m))
				 ((get-op-2 op) n m)]
		[((list '+ a 0)) a]
		[((list '+ 0 a)) a]
		[((list '* a 0)) 0]
		[((list '* 0 a)) 0]
		[((list '* a 1)) a]
		[((list '* 1 a)) a]
		[((list '/ a 1)) a]
		[((list 'mod a 1)) 0]
		[(e) e])
  (define (set-expr es tgt e)
    (match es
	   [(list w x y z)
	    (match tgt
		   ['w (list e x y z)]
		   ['x (list w e y z)]
		   ['y (list w x e z)]
		   ['z (list w x y e)])]))
  (define (subst es e)
    (match e
	   ['w (first es)]
	   ['x (second es)]
	   ['y (third es)]
	   ['z (fourth es)]
	   [(cons f args) (cons f (map (curry subst es) args))]
	   [n n]))
  (define init-es '(0 0 0 0))
  (define (inline es prog)
    (match prog
	   ['() es]
	   [(cons (list tgt e) rest)
	    (inline (set-expr es tgt (simplify (subst es e))) rest)]))
  (define/match (simp-if-eq-2 e)
		; I should have the symmetric clause, but was not needed for my input
		;[((list 'ifeq _ n a b)) #:when (and (number? n) (not (<= 0 n 1))) 0]
		; This is not very general, but was good enough for my input
		[((list 'ifeq (list 'ifeq s t 1 0) 0 c d)) (list 'ifeq s t d c)]
		[(_) e])
  (define/match (lift-if e)
		[((list f (list 'ifeq a0 b0 c0 d0) (list 'ifeq a1 b1 c1 d1)))
		 #:when (and (equal? a0 a1) (equal? b0 b1))
		 (list 'ifeq a0 b0 (list f c0 c1) (list f d0 d1))]
		[((list f (list 'ifeq a b c d) t)) (list 'ifeq a b (list f c t) (list f d t))]
		[((list f t (list 'ifeq a b c d))) (list 'ifeq a b (list f t c) (list f t d))]
		[(_) e])
  ; If we are testing equality with an input (in range 1..9), sometimes the
  ; test obviously will fail. This is not very general, but good enough for my
  ; input.
  (define/match (simp-if-input e)
		[((list 'ifeq (list '+ (list '% _ _) n) in t f))
		 #:when (and (is-input in) (number? n) (< 9 n))
		 f]
		[((list 'ifeq (list '+ in0 n) in1 t f))
		 #:when (and (is-input in0) (is-input in1) (number? n) (< 9 n))
		 f]
		[(_) e])
  (define (atom? a) (or (number? a) (symbol? a)))
  (define/match (simp-if-triv e)
		[((list 'ifeq a b t f)) #:when (and (atom? a) (atom? b) (equal? a b)) t]
		[((list 'ifeq a b t f)) #:when (and (number? a) (number? b)) (if (equal? a b) t f)]
		[(_) e])
  ; (a*m + b) % m = b
  ; It turns out that the symmetrical ones are not needed for my input
  (define/match (simp-mod-add-times e)
		[((list '% (list '+ (list '* a m1) b) m2)) #:when (equal? m1 m2) b]
		[(_) e])
  (define/match (simp-f2 e)
		[((list f1 (list f2 a n) m))
		 #:when (and (equal? f1 f2) (number? n) (number? m) (or (equal? f1 '+) (equal? f2 '*)))
		 (list f1 a ((get-op-2 f1) n m))]
		[(_) e])
  (define (simp e)
    (simp-f2
      (simp-mod-add-times
	(simp-if-triv
	  (simp-if-input
	    (lift-if
	      (simp-if-eq-2
		(const-fold e))))))))
  (define/match (simplify e)
    [((cons f args))
     (let ([s (simp (cons f (map simplify args)))])
       (if (equal? s e)
	 e
	 (simplify s)))]
    [(atom) atom])
  (define (split-prog prog)
    (define (loop prog)
      (if (null? prog)
	null
	(let-values ([(h t) (splitf-at (cdr prog) (lambda (i) (not (is-input (cadr i)))))])
	  (cons (cons (car prog) h) (loop t)))))
    (loop prog))
  (let* ([epochs (split-prog (pass1 0 prog))]
         [epochs-sb (map (curry inline '(w x y z)) epochs)])
    epochs-sb))

; Our input has the form where each of these blocks only cares about z
; and each one is either (Di are inputs, A, Ci are arbitrary numbers in range 0 < Ci < 26)
;   z <- 26*z + Di + Ci
;  or
;   z <- if (Di = (z % 26) - A) then (z/26) else (26*(z/26)+Di+ci)
(define (constraints prog)
  (let* ([epochs-full (compile prog)]
	 [epochs (map fourth epochs-full)])
    (when (memf (lambda (v) (member v '(w x y))) (flatten epochs))
      (error "epochs didn't only care about z"))
    (let ([parsed (for/list ([e epochs] [i (range (length epochs))])
			    (match e
				   [(list '+ (list '* 'z 26) (list '+ d c))
				    #:when (and (eq? d (string->symbol
							 (string-append "D" (number->string i))))
						(number? c)
						(< 0 c 26))
				    (list 'add-digit i c)]
				   [(list 'ifeq (list '+ (list '% 'z 26) a)
					  d
					  (list '/ 'z 26)
					  (list '+ (list '* (list '/ 'z 26) 26) (list '+ d1 c)))
				    #:when (and (eq? d d1)
						(eq? d (string->symbol
							 (string-append "D" (number->string i))))
						(number? a)
						(< -26 a 0)
						(number? c)
						(< 0 c 26))
				    (list 'ite i a c)]
		[_ (error "epoch not of expected form")]))])
      ; Our input has equal numbers of each form
      ; Thus, each "if" form must take the left branch, as we want to finish with z=0
      ; This gives constraints on the inputs Di
      (let-values ([(mul div) (partition (lambda (e) (eq? '+ (car e))) epochs)])
	(unless (eq? (length mul) (length div))
	  (error "expected equal numbers of both forms of epoch"))
      )
      ; Now collect and solve constraints.
      ; We only get constraints of the form Di = Dj + k, when Di is the input
      ; that is of the second form popping the corresponding first-form Dj off
      ; the stack.
      (letrec ([loop (match-lambda** ; args: stack, epochs
		       [('() '()) '()]
		       [(stk (cons (list 'add-digit j k) epochs))
			(loop (cons (list j k) stk) epochs)]
		       [((cons (list j k) stk) (cons (list 'ite i l _) epochs))
			(cons (list i j (+ k l)) (loop stk epochs))])])
	(loop '() parsed)))))

(define (part1 data)
  ; Grab the constraints, of the form
  ;  (list i j k)  meaning Di = Dj + k
  ; We maximise the model number simply by maximising each Di.
  ; NB: that D0 is the most significant digit, i.e. the 10^13 position
  (for/sum ([c (constraints data)])
	   (match c
		  [(list i j k) #:when (<= 0 k)
				(+ (* (expt 10 (- 13 i)) 9) (* (expt 10 (- 13 j)) (- 9 k)))]
		  [(list i j k) #:when (> 0 k)
				(+ (* (expt 10 (- 13 j)) 9) (* (expt 10 (- 13 i)) (+ 9 k)))])))

(printf "part 1: ~a\n"
	(part1 data))

(define (part2 data)
  ; Like part1, but minimising the number
  (for/sum ([c (constraints data)])
	   (match c
		  [(list i j k) #:when (<= 0 k)
				(+ (* (expt 10 (- 13 i)) (add1 k))
				   (* (expt 10 (- 13 j)) 1))]
		  [(list i j k) #:when (> 0 k)
				(+ (* (expt 10 (- 13 j)) (add1 (- k)))
				   (* (expt 10 (- 13 i)) 1))])))

(printf "part 2: ~a\n"
	(part2 data))
