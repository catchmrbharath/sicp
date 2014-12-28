;; Exercise 2.17
;; (last-pair (list 23 72 149 34))
;; (34)

(define (last-pair items)
  (if (null? (cdr items))
    items
    (last-pair (cdr items))))

(last-pair (list 23 72 149 34 ))


;;Exercise 2.18

(define (reverse a)
  (define (reverse-iter in out)
    (if (null? in)
        out
        (reverse-iter (cdr in) (cons (car in) out))))
  (reverse-iter a nil))

;;Exercise 2.19

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

(define (first-denomination xlist) (car xlist))

(define (except-first-denomination xlist) (cdr xlist))

(define (no-more? xlist) (null? xlist))

(define us-coins (list 50 25 10 5 1))

(cc 100 us-coins)

(define (same-parity elem . xlist)
  (define (same-parity-iter res-list num-list)
    (if (null? num-list)
      res-list
      (if (= (remainder (+ elem (car num-list)) 2) 0)
        (same-parity-iter (cons (car num-list) res-list) (cdr num-list))
        (same-parity-iter res-list (cdr num-list)))))
  (reverse (same-parity-iter (list elem) xlist)))

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))

;; Exercise 2.21
;; without using map
(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
    nil
    (cons (square (car items))
          (square-list (cdr items)))))

;;with using map
(define (square-list items)
  (map (lambda (x) (* x x))
       items))

;;Exercise 2.22
;; Iterative square list

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
  (iter items nil))


;; Exercise 2.23

(define (for-each proc items)
  (cond ((null? items) #t)
        (else (proc (car items))
              (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 68))



