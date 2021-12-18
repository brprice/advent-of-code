#lang racket

(require racket/generator)

(define (to-bits s)
  (define (f x)
    (let*-values ([(q0 b0) (quotient/remainder x 2)]
		  [(q1 b1) (quotient/remainder q0 2)]
		  [(q2 b2) (quotient/remainder q1 2)]
		  [(b3) (remainder q2 2)])
      (list b3 b2 b1 b0)))
  (apply append (map (lambda (c) (f (string->number (list->string (list c)) 16))) (string->list s))))

(define (with-data f)
  (call-with-input-file
    "../data/day16"
    (lambda (p)
      (let* ([lines (in-lines p)]
             [bits (to-bits (sequence-ref lines 0))])
	(f bits)))))

(define (with-test-data f)
  (let ([data '("38006F45291200"
		"EE00D40C823060"
		"8A004A801A8002F478"
                "620080001611562C8802118E34"
		"C0015000016115A2E0802F182340"
		"A0016C880162017C3686B18A3D4780"
		)])
    (map (compose f to-bits) data)))

(define (bitlist->number bs)
  (foldl (lambda (b acc) (+ (* 2 acc) b)) 0 bs))

(define (bits->number . bs)
  (bitlist->number bs))

; A parser is a map from strings to pairs of things and strings
; Packets are represented by either
; - (list ver 'lit n)  for ver, n numbers
; - (list ver op (list packet ...)) for ver, op numbers and packet a packet
(define (parse-pkt p)
  (match p
	 [(list-rest v2 v1 v0 t2 t1 t0 cts)
	  (let ([v (bits->number v2 v1 v0)]
		[t (bits->number t2 t1 t0)])
	    (if (eq? t 4)
	      (let ([lr (parse-literal cts)])
		(cons (list v 'lit (car lr)) (cdr lr)))
	      (if (eq? (car cts) 0)
		(let*-values ([(len-bits x) (split-at (cdr cts) 15)]
			      [(len) (bitlist->number len-bits)]
			      [(sub rest) (split-at x len)])
		  (cons (list v t (parse-pkts sub)) rest))
		(let*-values ([(pkts-bits x) (split-at (cdr cts) 11)]
			      [(pkts) (bitlist->number pkts-bits)]
			      [(sub-rest) (parse-n-pkts pkts x)])
		  (cons (list v t (car sub-rest)) (cdr sub-rest))))))]))

(define (parse-literal bs)
  (let loop ([n 0])
    (let*-values ([(h t) (split-at bs 5)]
		  [(m) (bitlist->number (cdr h))]
		  [(nm) (+ (* 16 n) m)])
      (set! bs t)
      (if (eq? 0 (car h))
	(cons nm bs)
	(loop nm)))))

(define (parse-pkts bs)
  (sequence->list
    (in-generator
      (let loop ([pr (parse-pkt bs)])
	(yield (car pr))
	(when (not (null? (cdr pr))) (loop (parse-pkt (cdr pr))))))))

(define (parse-n-pkts n bs)
  (let ([ps (for/list ([_ n])
		      (let ([pr (parse-pkt bs)])
			(set! bs (cdr pr))
			(car pr)))])
    (cons ps bs)))

(define (part1 p)
  (define (add-versions p)
    (match p
	   [(list v 'lit _) v]
	   [(list v _ ps) (apply + v (map add-versions ps))]))
  (add-versions (car (parse-pkt p))))

(printf "part 1: ~a\n"
	(with-data part1))
