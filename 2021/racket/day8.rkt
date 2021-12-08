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
