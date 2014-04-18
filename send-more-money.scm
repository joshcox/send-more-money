(load "mk-util.scm")

;;  S E N D
;;+ M O R E
;;_________
;;M O N E Y

;; x + y = r + 10*c
(define half-base10-addero
  (lambda (x y r c)
    (fresh (full)
      (pluso x y full)
      (conde
        ((<=o full (build-num 9)) (== full r) (== '() c))
        ((<o (build-num 9) full) (minuso full (build-num 10) r) (== c '(1)))))))

;; b + x + y = r + 10*c
(define full-base10-addero
  (lambda (b x y r c)
    (fresh (w xy wb)
      (half-base10-addero x y w xy)
      (half-base10-addero w b r wb)
      (xor^o xy wb c))))

;; b + x + y = r + 10*c
(define add/carryo
  (lambda (b x y r c)
    (fresh (full)
      (addero b x y full)
      (conda
        ((<=o full (build-num 9)) (== full r) (== 0 c))
        ((<o (build-num 9) full) (minuso full (build-num 10) r) (== 1 c))))))

(define smm
  (lambda (q)
    (fresh (s e n d m o r y)
      (fresh (c1 c2 c3 c4)
        (== q `(,s ,e ,n ,d ,m ,o ,r ,y))
        (=/=* s e n d m o r y)
        (== m '(1))
        (withino s            '(1) '(1 0 0 1))
        (withino e n d o r y  '()  '(1 0 0 1))
        (add/carryo 0  d e y c1)
        (add/carryo c1 n r e c2)
        (add/carryo c2 e o n c3)
        (add/carryo c3 s m o 1 )))))

(define smm2
  (lambda (q)
    (fresh (s e n d m o r y)
      (fresh (c1 c2 c3 c4)
        (== q `(,s ,e ,n ,d ,m ,o ,r ,y))
        (=/=* s e n d m o r y)
        (== m '(1))
        (membero? s (range 1 9))
        (membero? e (range 0 9))
        (membero? n (range 0 9))
        (membero? d (range 0 9))
        (membero? o (range 0 9))
        (membero? r (range 0 9))
        (membero? y (range 0 9))
        (add/carryo 0  d e y c1)
        (add/carryo c1 n r e c2)
        (add/carryo c2 e o n c3)
        (add/carryo c3 s m o 1 )))))

(define smm3
  (lambda (q)
    (fresh (s e n d m o r y)
      (fresh (c1 c2 c3 c4)
        (== q `(,s ,e ,n ,d ,m ,o ,r ,y))
        (=/=* s e n d m o r y)
        (== m '(1))
        (domain s (range 1 9))
        (domain e (range 0 9))
        (domain n (range 0 9))
        (domain d (range 0 9))
        (domain o (range 0 9))
        (domain r (range 0 9))
        (domain y (range 0 9))
        ;(withino s            '(1) '(1 0 0 1))
        ;(withino e n d o r y  '()  '(1 0 0 1))
        (full-base10-addero '() d e y c1)
        (full-base10-addero c1  n r e c2)
        (full-base10-addero c2  e o n c3)
        (full-base10-addero c3  s m o m )))))

;; smm:
;; > (time (run 1 (q) (smm q)))
;; (time (run 1 ...))
;;     301 collections
;;     251205 ms elapsed cpu time, including 282 ms collecting
;;     251199 ms elapsed real time, including 264 ms collecting
;;     2537202656 bytes allocated, including 2529955328 bytes reclaimed
;; (((1 0 0 1) (1 0 1) (0 1 1) (1 1 1) (1) () (0 0 0 1) (0 1)))

;; smm2: using domain
;; > (time (run 1 (q) (smm2 q)))
;; (time (run 1 ...))
;;     5107 collections
;;     1549934 ms elapsed cpu time, including 1256 ms collecting
;;     1549870 ms elapsed real time, including 1223 ms collecting
;;     42986044160 bytes allocated, including 42984246864 bytes reclaimed
;; (((1 0 0 1) (1 0 1) (0 1 1) (1 1 1) (1) () (0 0 0 1) (0 1)))

;; > (time (run 1 (q) (smm2 q)))
;; (time (run 1 ...))
;;     5181 collections
;;     3255041 ms elapsed cpu time, including 1234 ms collecting
;;     3254908 ms elapsed real time, including 1200 ms collecting
;;     43604060032 bytes allocated, including 43607436944 bytes reclaimed
;; (((1 0 0 1) (1 0 1) (0 1 1) (1 1 1) (1) () (0 0 0 1) (0 1)))
;old:
;> (time (run 1 (g) (smm g)))
;(time (run 1 ...))
;    30634 collections
;    1461088 ms elapsed cpu time, including 11151 ms collecting
;    1461061 ms elapsed real time, including 11199 ms collecting
;    258019576544 bytes allocated, including 258015377456 bytes reclaimed
;(((1 0 0 1) (1 0 1) (0 1 1) (1 1 1) (1) () (0 0 0 1) (0 1)));
;
;Solution (in 1461 seconds):
;    9 5 6 7
;  + 1 0 8 5
;  1 0 6 5 2
