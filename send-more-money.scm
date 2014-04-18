(load "mk-util.scm")

(define (t) (time (run 1 (q) (smm q))))

;; x + y = r + 10*c
(define half-base10-addero
  (lambda (x y r c)
    (fresh (full)
      (pluso x y full)
      (conda
        ((<=o full (build-num 9)) (== full r) (== '() c))
        ((<o (build-num 9) full) (minuso full (build-num 10) r) (== c '(1)))))))

;; b + x + y = r + 10*c
(define full-base10-addero
  (lambda (b x y r c)
    (fresh (w xy wb)
      (half-base10-addero x y w xy)
      (half-base10-addero w b r wb)
      (xoro xy wb c))))

;;  S E N D
;;+ M O R E
;;_________
;;M O N E Y
(define smm
  (lambda (q)
    (fresh (s e n d m o r y)
      (fresh (c1 c2 c3)
        (== q `(,s ,e ,n ,d ,m ,o ,r ,y))
        (=/=* s e n d m o r y)
        (== m '(1))
        ;(conde ((== s (build-num 8))) ((== s (build-num 9))))
        (domain s (range 8 9))
        (domain e (range 0 9))
        (domain n (range 0 9))
        (domain d (range 0 9))
        (domain o (range 0 9))
        (domain r (range 0 9))
        (domain y (range 0 9))
        (full-base10-addero c3  s m o m)
        (full-base10-addero c2  e o n c3)
        (full-base10-addero c1  n r e c2)
        (full-base10-addero '() d e y c1)))))
