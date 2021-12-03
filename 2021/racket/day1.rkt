#lang racket

(define (with-data f)
  (call-with-input-file "../data/day1"
			(lambda (p)
			  (f
			    (sequence->stream
			      (sequence-map
				string->number
				(in-lines p)))))))

(define (with-test-data f)
  (f (list
       199
       200
       208
       210
       200
       207
       240
       269
       260
       263)))

; NB: reading from a port-sequence is side-effecting (causes the bytes to be
; read from the port), so doing a
; (sequence-map f (in-parallel s (sequence-tail s 1))))))
; on the input "1\n2\n3\n4" will not give '((f 1 2) (f 2 3) (f 3 4))
; However, for a list-sequence, it is not side-effecting, which can be a
; confusing difference!
;
; This also bites when using (sequence-ref s 0) to extract the head:
; doing (sequence-ref s 0) a second time will give different answers depending
; on if 's' is a list or a port!
;
; The solution is to work over streams, not sequences, as using stream-first
; will lazily draw an element from the sequence and cache it, so it is
; idempotent.
; sequence->stream will do this conversion (in with-data)
(define (part1 s)
  (let ([f (lambda (a b) (if (< a b) #t #f))])
    (sequence-count
      identity
      (sequence-map f (in-parallel s (sequence-tail s 1))))))

(printf "part 1: ~a\n"
	(with-data part1))

(define (part2 s)
  (part1 (sequence-map +
		       (in-parallel s
				    (sequence-tail s 1)
				    (sequence-tail s 2)))))


(printf "part 2: ~a\n"
	(with-data part2))
