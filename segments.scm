(define (make-segment s e)
  (cons s e))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (midpoint-segment seg)
  (let ((s (start-segment seg))
        (e (end-segment seg)))
    (make-point (/ (+ (x-point s)
                      (x-point e))
                   2)
                (/ (+ (y-point s)
                      (y-point e))
                   2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))



;; Execrise 2.2

(define (make-rectangle ll ur)
  (cons ll ur))

(define (low-left-point rec)
  (car rec))

(define (up-right-point rec)
  (cdr rec))

(define (x-dist p1 p2)
  (abs (- (x-point p1)
          (x-point p2))))

(define (y-dist p1 p2)
  (abs (- (y-point p1)
          (y-point p2))))

(define (width rec)
  (let ((ll (low-left-point rec))
        (ur (up-right-point rec)))
    (x-dist ll ur)))

(define (heigth rec)
  (let ((ll (low-left-point rec))
        (ur (up-right-point rec)))
    (y-dist ll ur)))

(define (rec-perimeter rec)
  (let ((a (width rec))
        (b (heigth rec)))
    (* 2
       (+ a b))))

(define (rec-square rec)
  (let ((a (width rec))
        (b (heigth rec)))
    (* a b)))


