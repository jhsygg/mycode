(defun add (x y)
  (if (= x 0)
      y
      (+ (1- x) (1+ y))))

(defun +vect (v1 v2)
  (make-vector
   (+ (xcor v1) (xcor v2))
   (+ (ycor v1) (ycor v2))))

(defun scale-list (s l)
  (if (null l)
      nil
      (cons (* (car l) s)
	    (scale-list s (cdr l)))))


