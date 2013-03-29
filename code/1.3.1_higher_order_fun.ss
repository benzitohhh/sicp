(require racket/trace)

(define (cube n)
  (* n n n))


;; sum integers from a to b inclusive.
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

;; sum cubes from from a to b inclusive.
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))


;; sum of terms that converges slowly to pi/8
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We can abstract out the "summing" as follows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; Then the cube example from above becomes
(define (inc n) (+ n 1))
(define (sum-cubes-new a b)
  (sum cube a inc b))

;; With an identity function, we can compute sums of integers
(define (identity x) x)
(define (sum-integers-new a b)
  (sum identity a inc b))
(sum-integers-new 1 10)

;; And similarly for pi-sum:
(define (pi-sum-new a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;; Approximate integral of a function between [a, b]
;; (for some small value of dx)
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(integral cube 0 1 0.01)

;; and here it is iteratively
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ a result))))
  (iter a 0))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; And here's a way of abstacting out a "product"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (fact b)
  (product identity 1 inc b))

(define (pi-prod-term a)
  (/ (* (/ a (+ a 1.0))
        (+ a 2.0))
     (+ a 1.0)))

(define (pi-prod b)
  (define (inc2 a) (+ a 2))
  (product pi-prod-term 2 inc2 b))

;; This is an iterative
;; (i.e. tail-recursive) version of "product"
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Both sum and product are examples of a more general
;; notion called "accumulate"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (term a)
       (accumulate combiner null-value term (next a) next b))))

(define (sum-integers-acc a b)
  (accumulate + 0 identity a inc b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; And an iterative version of accumulate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner a result))))
  (iter a null-value))

(define (sum-integers-acc-iter a b)
  (accumulate-iter + 0 identity a inc b))

(define (factorial-acc-iter b)
  (accumulate-iter * 1 identity 1 inc b))

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (filtered-term a)
    (if (filter a)
        (term a)
        (null-value)))
  (if (> a b)
      null-value
      (combiner
       (filtered-term a)
       (accumulate combiner null-value term (next a) next b))))
