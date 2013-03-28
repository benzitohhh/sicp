(require racket/trace)

;; at each iteration, we assign
;; a <- a + b
;; b <- a

(define (fib-iter-outer n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(fib-iter-outer 5)

;; > ("iter:" 5 "a:" 1 "b:" 0)
;; > ("iter:" 4 "a:" 1 "b:" 1)
;; > ("iter:" 3 "a:" 2 "b:" 1)
;; > ("iter:" 2 "a:" 3 "b:" 2)
;; > ("iter:" 1 "a:" 5 "b:" 3)
;; > ("iter:" 0 "a:" 8 "b:" 5)
