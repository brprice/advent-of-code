#lang racket

(define (with-data f)
  (call-with-input-file
    "../data/day10"
    (compose f (curry stream-map string->list) sequence->stream in-lines)))

(define (with-test-data f)
  (let ([data #<<EOF
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
EOF
])
    (f (string-split data "\n"))))

(define (is-open c)
  (member c '(#\( #\[ #\{ #\<)))

(define/match (matching-open c)
  [(#\)) #\(]
  [(#\]) #\[]
  [(#\}) #\{]
  [(#\>) #\<])

; either returns
; for an corrupted line (the second elt is the unexpected character #\) #\} #\> or #\]): (list 'corrupted #\))
; for an incomplete line (the second elt is the unmatched opening characters, right-most first) (list 'incomplete stk)
(define (validate l)
  (let ([ret #f]
	[stk '()])
    (for ([c l]
	  #:break ret)
	 (if (is-open c)
	   (set! stk (cons c stk))
	   (if (eq? (matching-open c) (car stk))
	     (set! stk (cdr stk))
	     (set! ret (list 'corrupted c)))))
    (if ret ret (list 'incomplete stk))))

(define/match (score-corrupted c)
  [(#\)) 3]
  [(#\]) 57]
  [(#\}) 1197]
  [(#\>) 25137])

(define (part1 xs)
  (stream-fold + 0
	 (stream-map (compose score-corrupted cadr)
	      (stream-filter (lambda (r) (eq? 'corrupted (car r)))
		      (stream-map validate xs)))))

(printf "part 1: ~a\n"
	(with-data part1))

(define (score-incomplete unmatched)
  (define/match (f c)
    [(#\() 1]
    [(#\[) 2]
    [(#\{) 3]
    [(#\<) 4])
  (foldl (lambda (c acc) (+ (* 5 acc) (f c))) 0 unmatched))

(define (part2 xs)
  (define (middle xs)
    (list-ref xs (quotient (length xs) 2)))
  (middle (sort
	    (stream->list (stream-map (compose score-incomplete cadr)
				      (stream-filter (lambda (r) (eq? 'incomplete (car r)))
						     (stream-map validate xs))))
	    <)))

(printf "part 2: ~a\n"
	(with-data part2))
