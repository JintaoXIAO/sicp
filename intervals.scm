
(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (upper-bound y)))
        (p4 (* (upper-bound x) (lower-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))



(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b)
  (cons a b))

;; Exercise 2.7

(define (upper-bound i)
  (cdr i))

(define (lower-bound i)
  (car i))

;; Exercise 2.8

(define (sub-interval a b)
  (let ((la (lower-bound a))
        (ua (upper-bound a))
        (lb (lower-bound b))
        (ub (upper-bound b)))
    (make-interval (- la ub)
                   (- ua lb))))

;; Exercise 2.9

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))
;; ignore easy part


;; Exercise 2.10


(define (div-interval-fixed a b)
  (if (= 0 (width b))
      (display "zero interval divided")
      (div-interval a b)))

;; Exercise 2.11

(define (mul-interval-ben a b)
  (let ((la (lower-bound a))
        (ua (upper-bound a))
        (lb (lower-bound b))
        (ub (upper-bound b)))
    (cond ((and (< la 0 ua) (< lb 0 ub)) (make-interval (min (* la ub) (* lb ua))
                                                        (max (* la lb) (* ua ub))))
          ((and (< la 0 ua) (> lb 0)) (make-interval (* la ub) (* ua ub)))
          ((and (< la 0 ua) (< ub 0)) (make-interval (* ua lb) (* la lb)))
          ((and (> la 0) (< lb 0 ub)) (make-interval (* ua lb) (* ua ub)))
          ((and (> la 0) (> lb 0)) (make-interval (* la lb) (* ua ub)))
          ((and (> la 0) (< ub 0)) (make-interval (* ua lb) (* la ub)))
          ((and (< ua 0) (< lb 0 ub)) (make-interval (* la ub) (* la lb)))
          ((and (< ua 0) (> lb 0)) (make-interval (* la ub) (* ua lb)))
          ((and (< ua 0) (< ub 0)) (make-interval (* ua ub) (* la lb))))))

