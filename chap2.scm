(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; (define make-rat cons)
(define numer car)
(define denom cdr)


(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;; exe 2.1
(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (> d 0)
        (cons (/ n g) (/ d g))
        (cons (/ (- n) g) (/ (- d) g)))))


(print-rat (make-rat -1 3))
(print-rat (make-rat -1 -23))
(print-rat (make-rat 1 -3))
(print-rat (make-rat 1 3))
