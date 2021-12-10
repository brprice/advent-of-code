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
; for an incomplete line (list 'incomplete)
(define (validate l)
  (let ([ret '(incomplete)]
	[stk '()])
    (for ([c l]
	  #:break (not (equal? ret '(incomplete))))
	 (if (is-open c)
	   (set! stk (cons c stk))
	   (if (eq? (matching-open c) (car stk))
	     (set! stk (cdr stk))
	     (set! ret (list 'corrupted c)))))
    ret))

(define/match (score c)
  [(#\)) 3]
  [(#\]) 57]
  [(#\}) 1197]
  [(#\>) 25137])

(define (part1 xs)
  (stream-fold + 0
	 (stream-map (compose score cadr)
	      (stream-filter (lambda (r) (not (equal? '(incomplete) r)))
		      (stream-map validate xs)))))

(printf "part 1: ~a\n"
	(with-data part1))
