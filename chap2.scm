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

;; test 2.1
;;(print-rat (make-rat -1 3))
;;(print-rat (make-rat -1 -23))
;;(print-rat (make-rat 1 -3))
;;(print-rat (make-rat 1 3))

;; exe 2.2

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment seg)
  (let ((a (start-segment seg))
        (b (end-segment seg)))
      (make-point (/ 2 (+ (x-point a) (x-point b)))
                  (/ 2 (+ (y-point a) (y-point b))))))

;; test 
;;(define a (make-point 1 2))
;;(define b (make-point 8 9))
;;(define s (make-segment a b))
;;(define m (midpoint-segment s))
;;(print-point m)

;; exe 2.3 
;; l: left-low-point
;; r: right-up-point
(define (make-ractangle l r)
  (cons l r))

(define (left-low-point rect)
  (car rect))

(define (right-up-point rect)
  (cdr rect))

(define (x-distance p1 p2)
  (abs (- (x-point p1) (x-point p2))))

(define (y-distance p1 p2)
  (abs (- (y-point p1) (y-point p2))))

(define (horizontal-border-length rect)
  (let ((ll (left-low-point rect))
        (ru (right-up-point rect)))
      (x-distance ll ru)))

(define (vertical-border-length rect)
  (let ((ll (left-low-point rect))
        (ru (right-up-point rect)))
      (y-distance ll ru)))

;; test
;;(define l (make-point 1 1))
;;(define r (make-point 5 9))

;;(define rect (make-ractangle l r))
;;(define a (horizontal-border-length rect))
;;(define b (vertical-border-length rect))

(define (cons1 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS " m))))
  dispatch)

(define (car1 z) (z 0))
(define (cdr1 z) (z 1))

;; exe2.4
(define (cons2 x y)
  (lambda (m) (m x y)))

(define (car2 z)
  (z (lambda (p q) p)))

(define (cdr2 z)
  (z (lambda (p q) q)))

;; test
;;(define c (cons2 1 2))

;; exe2.5 TODO

;; exe2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((ll (* (lower-bound x) (lower-bound y)))
        (lu (* (lower-bound x) (upper-bound y)))
        (ul (* (upper-bound x) (lower-bound y)))
        (uu (* (upper-bound x) (upper-bound y))))
  (make-interval (min ll lu ul uu)
                 (max ll lu ul uu))))


(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound))
                  (/ 1.0 (lower-bound)))))


;; exe2.7

(define (make-interval a b)
  (cons a b))

(define (lower-bound a)
  (car a))

(define (upper-bound a)
  (cdr a))

;; exe2.8

(define (sub-interval a b)
  (add-interval a
                (make-interval (- (upper-bound b))
                               (- (lower-bound b)))))

(define (display-interval a)
  (newline)
  (display "[")
  (display(lower-bound a))
  (display ", ")
  (display (upper-bound a))
  (display "]"))

;; test
(define a (make-interval -1 5))
(define b (make-interval -3 3))

;;(display-interval a)
;;(display-interval b)

;;(define s (sub-interval a b))
;;(display-interval s)

;; exe2.9
(define (interval-width a)
  (/ (- (upper-bound a) (lower-bound a))
     2.0))

;;(display (interval-width a))

;; skip..... TODO add skipped content in future!!!

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (lengthi items)
  (define (length-iter result rest)
    (if (null? rest)
        result
        (length-iter (+ result 1) (cdr rest))))
  (length-iter 0 items))

(define (append1 list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append1 (cdr list1) list2))))

;; exe2.17
(define (last-pair items)
  (cond ((null? items) '())
        ((null? (cdr items)) (car items))
        (else (last-pair (cdr items)))))

;; exe2.18
(define (reverse1 items)
  (if (null? items)
      '()
      (append (reverse1 (cdr items))
              (car items))))

;; exe2.19  skip TODO


(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (map1 proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map1 proc (cdr items)))))

(define (scale-listm items factor)
  (map (lambda (x) (* x factor))
       items))

;;exe2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-listm items)
  (map square items))

;;exe2.22
(define (square-listi items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
               (square (car things))))))
  (iter items '()))

;;exe2.23
(define (for-each1 proc items)
  (if (not (null? items))
      (proc (car items)))
  (if (not (null? items))
      (for-each1 proc (cdr items))))


(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

;;exe2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

;;exe2.27
(define (deep-reverse items)
  (cond ((null? items) '())
        ((not (pair? items)) items)
        (else (append (deep-reverse (cdr items))
                      (cons (deep-reverse (car items)) '())))))


;;test
;;(define r (deep-reverse (list 1 2 3 (list 5 6 7))))

;;exe2.28
(define (fringe tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (list (fringe (car tree)))
                      (fringe (cdr tree))))))

;;test
;;(fringe (list (list 1 2) (list 3 (list 4 (list 5 6)))))


;;exe2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (if (not (pair? mobile))
      mobile
      (let ((lb (left-branch mobile))
            (rb (right-branch mobile)))
        (+ (if (pair? lb) (total-weight (branch-structure lb)) lb)
           (if (pair? rb) (total-weight (branch-structure rb)) rb)))))


(define m (make-mobile
           (make-branch 1 (make-mobile (make-branch 2 3)
                                       (make-branch 4 5)))
           (make-branch 6 7)))

(define (balanced mobile)
  (define (balance-weight structure)
    (if (pair? structure)
        (* (branch-length structure) (+ (balance-weight (left-branch (branch-structure structure)))
                                        (balance-weight (right-branch (branch-structure structure)))))
        structure))
  (=? (branch-structure (left-branch mobile))
      (branch-structure (right-branch mobile))))


(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair?)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-treem tree factor)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree))
             (* sub-tree factor)
             (scale-treem sub-tree factor)))
       tree))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree))
             (square sub-tree)
             (square-tree sub-tree)))
       tree))

;;test
;;(square-tree (list 1 (list 2 (list 3 4) 5)
;; (list 6 7)))

;;exe2.31
(define (tree-map op tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree))
             (op sub-tree)
             (tree-map op sub-tree)))
       tree))

(define (square-treem tree)
  (tree-map square tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;;exe2.32
(define (subsets s)
  (if (null? s)
      '()
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (e) (cons e (car s))) rest)))))

;;test
;;(subsets '(1 2 3)) not work cause map doesn't work on an empty list

(define (filter1 predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;exe2.33
(define (mapa p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (appenda seq1 seq2)
  (accumulate cons seq2 seq1))

(define (lengtha sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;;exe2.34
(define (horner-eval x cofficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              cofficient-sequence))

;;test
;;(horner-eval 2 '(1 3 0 5 0 1))

;;exe2.35
(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y))
              0
              (map (lambda (subtree)
                     (if (not (pair? subtree))
                         1
                         (count-leaves subtree)))
                   t)))

;;test
;;(define t (list (list 1 2) (list 4 5)))
;;count-leaves t)
