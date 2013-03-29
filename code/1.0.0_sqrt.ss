;; Suppose we want to find x s.t. f(x)=0.
;; Netwon's method is to take a guess.
;; Then improve the guess by following the gradient
;; down to the x axis using
;; xn1 = xn - f(xn)/f'(xn)
;; Repeat until sufficiently close

;; Suppose we want to find x st x^2 = S
;; This is equivalent to x^2 - S = 0
;; We can then apply Newton's method above,
;; yielding xn1 = 0.5 (xn + S/xn)

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
    (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(sqrt 400)
