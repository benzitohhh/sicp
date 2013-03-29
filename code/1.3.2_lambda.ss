(require racket/trace)

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (pi-sum b)
  (sum-iter
   (lambda (x)(/ 1.0 (* x (+ x 2))))
   1
   (lambda (x) (+ x 4))
   b))

(define (integral f a b dx)
  (* (sum-iter f
               (+ a (/ dx 2.0))
               (lambda (x) (+ x dx))
               b)
     dx))

(define (cube x) (* x x x))
(integral cube 0 1 0.00001)

;; We can use "lanbda" to "bind" local variables
(define (f x y)
  ((lambda (a b)
     (+ (* x (square a)) (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

;; Which is equivalent to the "let" special form:
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; NOTE: the let variables themselves are computed
;; outside the let:
(define x 2)
(let ((x 3)
      (y (+ x 2)))
  (* x y))

(define (square x) (* x x))
(define (f g) (g 2))
(f square)
(f (lambda (z) (* z (+ z 1))))
