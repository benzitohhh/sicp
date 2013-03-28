(require racket/trace)

;; naive prime finding algorithm
;; runs in o(n^0.5)

(define (square n) (* n n))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest_divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest_divisor n)))


;; Below is code for testing the first 3 primes above n.
;; The time to find each is also displayed

;; TODO: rewrite using (time expr) form

;; Since the search takes approx On^0.5...
;; Moving from start=1000 to start=1000000 should
;; increase the runtime by a factor of approx 1000^0.5 (i.e. 31)

(define (search-for-primes n)
  (define (iter n counter)
    (if (< counter 3)
        (if (timed-prime-test n)
            (iter (+ n 2) (+ counter 1))
            (iter (+ n 2) counter))
        true))
  (if (even? n)
      (iter (+ n 1) 0)
      (iter (+ n 2) 0)))

(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (current-milliseconds) start-time))
      false))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  true)

(search-for-primes 10000)
