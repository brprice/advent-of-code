#lang racket

(define test-data
  '((on (-20 26) (-36 17) (-47 7))
    (on (-20 33) (-21 23) (-26 28))
    (on (-22 28) (-29 23) (-38 16))
    (on (-46 7) (-6 46) (-50 -1))
    (on (-49 1) (-3 46) (-24 28))
    (on (2 47) (-22 22) (-23 27))
    (on (-27 23) (-28 26) (-21 29))
    (on (-39 5) (-6 47) (-3 44))
    (on (-30 21) (-8 43) (-13 34))
    (on (-22 26) (-27 20) (-29 19))
    (off (-48 -32) (26 41) (-47 -37))
    (on (-12 35) (6 50) (-50 -2))
    (off (-48 -32) (-32 -16) (-15 -5))
    (on (-18 26) (-33 15) (-7 46))
    (off (-40 -22) (-38 -28) (23 41))
    (on (-16 35) (-41 10) (-47 6))
    (off (-32 -23) (11 30) (-14 3))
    (on (-49 -5) (-3 45) (-29 18))
    (off (18 30) (-20 -8) (-3 13))
    (on (-41 9) (-7 43) (-33 15))
    (on (-54112 -39298) (-85059 -49293) (-27449 7877))
    (on (967 23432) (45373 81175) (27513 53682))))

(define (parse-range s)
  (let* ([s1 (cadr (string-split s "="))]
	 [mM (string-split s1 "..")]
	 [m (string->number (car mM))]
	 [M (string->number (cadr mM))])
    (list m M)))

(define data
  (for/list ([l (in-lines (open-input-file "../data/day22"))])
	    (let* ([s1 (string-split l " ")]
		   [parity (if (string=? "on" (car s1)) 'on 'off)]
		   [s2 (string-split (cadr s1) ",")]
		   [xr (parse-range (car s2))]
		   [yr (parse-range (cadr s2))]
		   [zr (parse-range (caddr s2))])
	      (list parity xr yr zr))))

; Trivial brute-force: loop over all cubes in the target region
; and see if they are lit
(define (part1 data)
  (define/match (relevant r)
		[((list _ (list xm xM) (list ym yM) (list zm zM)))
		 (not (or (< xM -50) (> xm 50)
			  (< yM -50) (> ym 50)
			  (< zM -50) (> zm 50)))])
  (define rel (reverse (filter relevant data)))
  (define ((ctg p) r)
    (match p
	   [(list x y z)
	    (match r
		   [(list _ (list xm xM) (list ym yM) (list zm zM))
		    (and (<= xm x xM) (<= ym y yM) (<= zm z zM))])]))
  (define (lit p)
    (match (findf (ctg p) rel)
	   [#f #f]
	   [(cons 'on _) #t]
	   [(cons 'off _) #f]))
  (for*/sum ([i (range -50 51)] [j (range -50 51)] [k (range -50 51)])
	    (if (lit (list i j k)) 1 0)))

(printf "part 1: ~a\n"
	(part1 data))
