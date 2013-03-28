(require racket/trace)

(define (pasc r c)
  (cond ((< c 0) 0)
        ((< r 0) 0)
        ((= c 0) 1)
        ((= c r) 1)
        (else (+ (pasc (- r 1) (- c 1))
                 (pasc (- r 1) c)))))

(pasc 3 3)

(define (print_row r c)
  (cond ((>= c 0)
         (print (pasc r c))
         (print_row r (- c 1)))))

(define (print_all n)
  (cond ((>= n 0)
         (print_row n n)
         (newline)
         (print_all (- n 1)))))

(print_all 5)
