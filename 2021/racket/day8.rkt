#lang racket

(define (with-data f)
  (call-with-input-file
    "../data/day8"
    (let ([g (lambda (l)
	       (match (string-split l " | ")
		      [(list in out) (cons (string-split in) (string-split out))]))])
      (compose f (curry sequence-map g) in-lines))))

(define (with-test-data f)
  (let ([data '(((be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb) . (fdgacbe cefdb cefbgd gcbe))
		((edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec) . (fcgedb cgb dgebacf gc))
                ((fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef) . (cg cg fdcagb cbg))
                ((fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega) . (efabcd cedba gadfec cb))
                ((aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga) . (gecf egdcabf bgf bfgea))
                ((fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf) . (gebdcfa ecba ca fadegcb))
                ((dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf) . (cefg dcbef fcge gbcadfe))
                ((bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd) . (ed bcgafe cdgba cbgef))
                ((egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg) . (gbdfcae bgc cg cgb))
                ((gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc) . (fgae cfgab fg bagce)))]
	[g (lambda (p) (cons (map symbol->string (car p)) (map symbol->string (cdr p))))])
    (f (map g data))))

(define (part1 xs)
  (let ([f (lambda (o) (member (string-length o) '(2 3 4 7)))])
    (sequence-fold + 0 (sequence-map (compose length (curry filter f) cdr) xs))))

(printf "part 1: ~a\n"
	(with-data part1))

; part 2: I manually did the analysis and worked out you can tell that, for
; example "3" is the unique 5-segment number which has all the segments of "1"
; illuminated. This code just runs this simple analysis.
(define (part2digits in)
  (let ([lits (sort (group-by string-length in) < #:key (lambda (g) (string-length (car g))))]
	[ctg (lambda (small) (lambda (big) (andmap (lambda (sg) (member sg (string->list big))) (string->list small))))])
    (match lits
	   [(list (list one) (list seven) (list four) two-three-five zero-six-nine (list eight))
	    (let* ([three (car (filter (ctg one) two-three-five))]
		   [six (car (filter (compose not (ctg one)) zero-six-nine))]
		   [five (car (filter (lambda (x) ((ctg x) six)) two-three-five))]
		   [zero (car (filter (compose not (ctg five)) zero-six-nine))]
		   [nine (car (filter (ctg four) zero-six-nine))]
		   [two (car (filter (lambda (x) (not ((ctg x) nine))) two-three-five))])
	      (list zero one two three four five six seven eight nine))])))

(define (part2help io)
  (let* ([srt (lambda (s) (sort (string->list s) char<?))]
	 [digits (map srt (part2digits (car io)))]
	 [out (map srt (cdr io))])
    (foldl (lambda (d acc) (+ (* 10 acc) d)) 0 (map (curry index-of digits) out))))

(define (part2 xs)
  (sequence-fold + 0 (sequence-map part2help xs)))

(printf "part 2: ~a\n"
	(with-data part2))
