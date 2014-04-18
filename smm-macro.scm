(load "mk-util.scm")

(define add/carryo2
  (lambda (n1 n2 cin cout o)
    (fresh (sum)
      (pluso* n1 n2 cin sum)
      (/o sum (build-num 10) cout o))))

(define (rem-dup x* pred?)
  (cond
    ((null? x*) '())
    ((memp (lambda (x) (pred? (car x*) x)) (cdr x*))
     (rem-dup (cdr x*) pred?))
    (else
      (cons (car x*) (rem-dup (cdr x*) pred?)))))

;;a fun macro that generates word problems of the sort
;;needs some work and to be updated with the newer version
;;of smm
(define-syntax word-play
  (lambda (x)
  (syntax-case x (:)
    ((_ () () () : (fv* ...) (c* ...) (with* ...) (w* ...) (a/c* ...))
     (with-syntax ([(fv* ...) 
                    (rem-dup (syntax->list #'(fv* ...)) free-identifier=?) ])
       #'(lambda (q)
           (fresh (fv* ...)
             (fresh (c* ...)
               (== q `(,(list fv* ...) fv* ...)) 
               (=/=* fv* ...) 
               (withino c* ... '() '(1)) 
               with* ... 
               (withino w* ... '() '(1 0 0 1))
               a/c* ...)))))
    ((_ (l1) (l2) (l3) : (fv* ...) (c* ... c) (with* ...) (w* ...) (a/c* ...))
     #'(word-play () () () : (fv* ... l1 l2 l3) (c* ... c) (with* ...) (w* ... l1 l2 l3) 
         ((add/carryo l1 l2 '() c l3) a/c* ...)))
    ((_ (l1 l1* ...) (l2 l2* ...) (l3 l3* ...) 
       : (fv* ...) (c* ... c) (with* ...) (w* ...) (a/c* ...))
     #'(word-play (l1* ...) (l2* ...) (l3* ...)
         : (fv* ... l1 l2 l3) (c* ... c c^) (with* ...) (l1 l2 l3 w* ...) 
           ((add/carryo l1 l2 c^ c l3) a/c* ...)))
    ((_ (l1 l1* ...) (l2 l2* ...) (l3 l3^ l3* ...))
     #'(word-play (l1* ...) (l2* ...) (l3* ...) 
         : (l1 l2 l3 l3^) (c) ((withino l1 l2 l3 '(1) '(1 0 0 1))) (l3^) 
           ((add/carryo l1 l2 c l3 l3^)))))))

;NEW MACRO ROAR!
;> (define chc (word-play (c a n) (h a z) (c a s h)))
;> (run 1 (q) (chc q))
;()
;> 
;
;> (define smm (word-play (s e n d) (m o r e) (m o n e y)))
;> (run 1 (q) (smm q))
;(((1 0 0 1) (1) () (0 1 1) (0 0 0 1) (1 1 1) (1 0 1) (0 1)))
;
;> (define a (word-play (a) (b) (c d)))
;> (run 1 (q) (a q))
;((((1 1) (1 0 0 1) (1) (0 1)) a b c d))
;> (run 2 (q) (a q))
;((((1 1) (1 0 0 1) (1) (0 1)) a b c d)
;  (((0 0 1) (1 0 0 1) (1) (1 1)) a b c d))
