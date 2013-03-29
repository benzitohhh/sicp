(require racket/trace)

(require racket/run)

;; exploit fermat's little theorem:
;; if p is prime, then a^p is congruent to a (mod p).
;; And specifically, for a<p: a^p (mod p) = a
;; So conversely....
;; A. if a^p (mod p) != a, then n is definitely not prime.
;; B. if a^p (mod p) = a, then n is *probably* prime... keep testing!

(define (square n) (* n n))

;; this returns base^exp (mod m)
;; NOTE 1: values reduced by mod m at every step
;;        (hence efficient even for extremely large inputs)
;; NOTE 2: square function is used rather than explicit multiply
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; this runs the fermat test for a randomly chosen base
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
