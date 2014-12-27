;;Section 2.1.4
;;Extended exercise: Interval Arithmetic

;; Constructor

(define (make-interval a b) (cons a b))

;; Helper functions
;; Exercise 2.7
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (print-interval z)
  (newline)
  (display "(")
  (display (lower-bound z))
  (display ", ")
  (display (upper-bound z))
  (display ")"))

;; Already defined functions

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                  (max p1 p2 p3 p4))))

(define (div-interval-naive x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define r1 (make-interval 2.5 3.5))
(define r2 (make-interval 4 6))

(print-interval (add-interval r1 r2))
(print-interval (mul-interval r1 r2))
(print-interval (div-interval r1 r2))

;;Exercise 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


(print-interval (sub-interval r1 r2))

;;Exercise 2.10
(define (div-interval x y)
  (if (and (<= (lower-bound y) 0)
           (>= (upper-bound y) 0))
    (error "Error can't divide interval with zero in it")
    (div-interval-naive x y)))

(define interval-with-zero (make-interval -3 3))
(div-interval r1 interval-with-zero)



