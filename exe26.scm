
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (add-1 zero)
  (lambda (f) (lambda (x) (f x))))

(define two (add-1 one)
  (lambda (f) (lambda (x) (f (f x)))))

(define three (add-1 two)
  (lambda (f) (lambda (x) (f (f (f x))))))

(define (++ m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))



