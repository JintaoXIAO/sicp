(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
              (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
              (* numer y) (denom x))
    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
    (* (denom x) (numer x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
    (* (denom x) (numer y))))


(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Exercise 2.1:
(define (make-rat-better n d)
  (if (> d 0)
      (make-rat n d)
      (make-rat (- n) (- d))))
