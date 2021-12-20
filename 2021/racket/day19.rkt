#lang racket

(require data/queue)

(define test-data
  '(((404 -588 -901) (528 -643 409) (-838 591 734) (390 -675 -793) (-537 -823 -458) (-485 -357 347)
     (-345 -311 381) (-661 -816 -575) (-876 649 763) (-618 -824 -621) (553 345 -567) (474 580 667)
     (-447 -329 318) (-584 868 -557) (544 -627 -890) (564 392 -477) (455 729 728) (-892 524 684)
     (-689 845 -530) (423 -701 434) (7 -33 -71) (630 319 -379) (443 580 662) (-789 900 -551)
     (459 -707 401))
    ((686 422 578) (605 423 415) (515 917 -361) (-336 658 858) (95 138 22) (-476 619 847)
     (-340 -569 -846) (567 -361 727) (-460 603 -452) (669 -402 600) (729 430 532) (-500 -761 534)
     (-322 571 750) (-466 -666 -811) (-429 -592 574) (-355 545 -477) (703 -491 -529) (-328 -685 520)
     (413 935 -424) (-391 539 -444) (586 -435 557) (-364 -763 -893) (807 -499 -711) (755 -354 -619)
     (553 889 -390))
    ((649 640 665) (682 -795 504) (-784 533 -524) (-644 584 -595) (-588 -843 648) (-30 6 44)
     (-674 560 763) (500 723 -460) (609 671 -379) (-555 -800 653) (-675 -892 -343) (697 -426 -610)
     (578 704 681) (493 664 -388) (-671 -858 530) (-667 343 800) (571 -461 -707) (-138 -166 112)
     (-889 563 -600) (646 -828 498) (640 759 510) (-630 509 768) (-681 -892 -333) (673 -379 -804)
     (-742 -814 -386) (577 -820 562))
    ((-589 542 597) (605 -692 669) (-500 565 -823) (-660 373 557) (-458 -679 -417) (-488 449 543)
     (-626 468 -788) (338 -750 -386) (528 -832 -391) (562 -778 733) (-938 -730 414) (543 643 -506)
     (-524 371 -870) (407 773 750) (-104 29 83) (378 -903 -323) (-778 -728 485) (426 699 580)
     (-438 -605 -362) (-469 -447 -387) (509 732 623) (647 635 -688) (-868 -804 481) (614 -800 639)
     (595 780 -596))
    ((727 592 562) (-293 -554 779) (441 611 -461) (-714 465 -776) (-743 427 -804) (-660 -479 -426)
     (832 -632 460) (927 -485 -438) (408 393 -506) (466 436 -512) (110 16 151) (-258 -428 682)
     (-393 719 612) (-211 -452 876) (808 -476 -593) (-575 615 604) (-485 667 467) (-680 325 -822)
     (-627 -443 -432) (872 -547 -609) (833 512 582) (807 604 487) (839 -516 451) (891 -625 532)
     (-652 -548 -490) (30 -46 -14))))

(define data
  (let ([scans '()] [cur '()])
    (for ([l (in-lines (open-input-file "../data/day19"))] #:unless (equal? "" l))
	(if (string-prefix? l "---" )
	  (unless (null? cur)
	    (set! scans (cons (reverse cur) scans))
	    (set! cur '()))
	  (match (string-split l ",")
		 [(list x y z)
		  (let ([nx (string->number x)]
			[ny (string->number y)]
			[nz (string->number z)])
		    (set! cur (cons (list nx ny nz) cur)))])))
    (set! scans (cons (reverse cur) scans))
    (reverse scans)))

(define (sub v w)
  (for/list ([x v] [y w])
	    (- x y)))

(define (neg v)
  (map - v))

; for the given orientation, give offset of b from a along with b's beacons
; relative to a; or #f if they don't match
(define (match-scans a b)
  (let ([ret #f])
    (for* ([p a] [q b])
	  #:break ret
	  (let* ([s (sub q p)]
		 [bshift (map (lambda (x) (sub x s)) b)]
		 [overlap (set-intersect a bshift)])
	    (when (<= 12 (length overlap)) (set! ret (list (neg s) bshift)))))
    ret))

(define (orientations xs)
  (define (flip-some ys)
    (map (lambda (c) (map (lambda (y) (if (member y c) (- y) y) ) ys))
	 (combinations ys)))
  (define ofs (append-map flip-some (permutations '(1 2 3))))
  (define (sgn p)
    (for*/product ([i (length p)] [j (range i (length p))])
		  (if (eq? i j)
		    (if (> 0 (list-ref p i)) -1 1)
		    (if (> (abs (list-ref p i)) (abs (list-ref p j))) -1 1))))
  (define os (filter (lambda (o) (eq? 1 (sgn o))) ofs))
  (define (run1 x) (lambda (i) (if (< 0 i)
				 (list-ref x (sub1 i))
				 (- (list-ref x (- -1 i))))))
  (define (run o) (lambda (x) (map (run1 x) o)))
  (for/list ([o os])
	    (map (run o) xs)))

; for a fixed first sensor, return offset of second from the first along with
; its beacons relative to the first sensor; or #f if they don't match
(define (match-some-orientation a b)
  (for*/first ([bo (orientations b)] [m (list (match-scans a bo))] #:when m)
	     m))

; Brute force: we (roughly) consider each pair of sensors. For each, consider
; each pair of (beacon-from-left, beacon-from-right) and see if they overlap
; enough. This lets us iteratively build the map.
(define (part1 scans)
  (let* ([known (list (car scans))]
	 [todo (make-queue)]
	 [step (lambda ()
	       (let ([scan (dequeue! todo)]
		     [done #f])
		 (for [(k known)] #:break done
		      (let ([m (match-some-orientation k scan)])
			(when m
			  (set! known (cons (cadr m) known))
			  (set! done #t))))
		 (unless done (enqueue! todo scan))))])
    (for-each (curry enqueue! todo) (cdr scans))
    (do () ((queue-empty? todo) (length (remove-duplicates (apply append known)))) (step))))

(printf "part 1: ~a\n"
	(part1 data))
