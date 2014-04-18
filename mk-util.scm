;;some nice utilities for miniKanren

(load "mk.scm")
(load "numbers.scm")

;;aps macro for pipelining a series of pluso operations
;;to a single variable
;;(pluso* x1 x2 x* ... o)
(define-syntax pluso*
  (syntax-rules ()
    ((_ (uf* ...) (n0 n1) o (p* ...))
     (fresh (uf* ...) p* ... (pluso n0 n1 o)))
    ((_ (uf* ...) (n0 n1 n* ...) o (p* ...))
     (pluso* (f1 uf* ...) (f1 n* ...) o ((pluso n0 n1 f1) p* ...)))
    ((_ n0 n1 n* ... o)
     (pluso* (f1) (f1 n* ...) o ((pluso n0 n1 f1))))))

;;=/=* (all-diff, unique, whatever) - written by Jason Hemann
(define-syntax =/=*
  (syntax-rules (:)
    ((_ : (dt dt* ...) () (cl* ...)) (fresh () cl* ...))
    ((_ t1 t* ... : (ot ot* ...) (dt* ...) (cl* ...))
     (=/=* t1 t* ... : (ot* ...) (ot dt* ...) ((=/= t1 ot) cl* ...)))
    ((_ t1 t* ... : () (dt dt* ...) (cl* ...))
     (=/=* t* ... : (t1 dt dt* ...) () (cl* ...)))
    ((_ t0 t1 t* ...) (=/=* t1 t* ... : (t0) () ()))))

;;(withino n n* ... lbd ubd)
;;a constraint that restricts n,n* to fall within lbd and ubd 
(define-syntax withino
  (syntax-rules (:)
    ((_ lbd ubd : (c* ...))
     (fresh () c* ...))
    ((_ n n* ... lbd ubd : (c* ...))
     (withino n* ... lbd ubd : ((<=o n ubd) (<=o lbd n) c* ...)))
    ((_ n n* ... lbd ubd)
     (withino n* ... lbd ubd : ((<=o n ubd) (<=o lbd n))))))

;;builds a list of Oleg numbers from lbd to ubd
(define (range lbd ubd)
  (cond
    ((> lbd ubd) '())
    (else (cons (build-num lbd) (range (add1 lbd) ubd)))))

;;similar to withino, this constrains a number appear within
;;a finite domain bounded by the provided list.
(define domain
  (lambda (x y*)
    (fresh (a d)
      (== `(,a . ,d) y*)
      (conde
        ((== x a))
        ((domain x d))))))

(define xoro
  (lambda (x y r)
    (conde
      ((== x '()) (== y '()) (== r '()))
      ((== x '()) (== y '(1)) (== r '(1)))
      ((== x '(1)) (== y '()) (== r '(1)))
      ((== x '(1)) (== y '(1)) (== r '())))))
