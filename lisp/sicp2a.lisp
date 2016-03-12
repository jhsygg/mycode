(defun sum-int (a b)
  (if (> a b)
      0
      (+ a
	 (sum-int (1+ a) b))))

(defun sum-sq (a b)
  (if (> a b)
      0
      (+ (square a)
	 (sum-sq (1+ a) b))))

(defun pi-sum (a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2)))
	 (pi-sum (+ a 4) b))))

(defun sum (term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term
	      (next a)
	      next
	      b))))

(defvar tolerance 0.00001)

(defun +rat (x y)
  (cons (+ (* (car x) (car (cdr y)))
	   (* (car y) (car (cdr x))))
	(* (car (cdr x))  (car (cdr y)))))

(defun numer (x)
  (let ((g (gcd (car x) (car (cdr x)))))
    (/ (car x) g)))

(defun denom (x)
  (let ((g (gcd (car x) (car (cdr x)))))
    (/ (car (cdr x)) g)))

(defun sqrt-iter (guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))

(defun sqrt1 (x)
  (sqrt-iter 1.0 x))

(defun factorial (n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

