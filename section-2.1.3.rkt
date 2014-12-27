;; A different way to represent cons
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1"))
          )))

(define (car z) (z 0))
(define (cdr z) (z 1))


;; Exercise 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))


;;Exercise 2.5

(define (cons x y)
  (* (exp 2 a) (exp 3 a)))

(define (car z)
  (max-power 2 z))

(define (cdr z)
  (max-power 3 z))


;; Exercise 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;;Sustitute model for one (add-1 zero)
;; (add-1 zero)
;; (lambda (f) (lambda (x) (f ((zero f) x))))
;; (lambda (f) (lambda (x) (f (lambda (x) x) x)))
;; (lambda (f) (lambda (x) (f x) ))

(define one (lambda (f) (lambda (x) (f x))))

;; Substitute model fot two (add-1 one)

;; (add-1 one)
;; (lambda (f) (lambda (x) (f ((one f) x))))
;; (lambda (f) (lambda (x) (f (lambda (x) (f x) x))))
;; (lambda (f) (lambda (x) (f (f x)))
;; Repeated application of the function f.

(define two (lambda (f) (lambda (x) (f (f x)))))

;; Defining addition
;; Adding is composing functions

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
