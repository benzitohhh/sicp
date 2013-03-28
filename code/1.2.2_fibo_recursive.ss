(require racket/trace)

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;;(trace fib)
;;(untrace fib)

(fib 5)

;; >(fib 5)
;; > (fib 4)
;; > >(fib 3)
;; > > (fib 2)
;; > > >(fib 1)
;; < < <1
;; > > >(fib 0)
;; < < <0
;; < < 1
;; > > (fib 1)
;; < < 1
;; < <2
;; > >(fib 2)
;; > > (fib 1)
;; < < 1
;; > > (fib 0)
;; < < 0
;; < <1
;; < 3
;; > (fib 3)
;; > >(fib 2)
;; > > (fib 1)
;; < < 1
;; > > (fib 0)
;; < < 0
;; < <1
;; > >(fib 1)
;; < <1
;; < 2
;; <5
;; 5
;; >
