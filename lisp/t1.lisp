(dotimes (x 9)
  (dotimes (y 9)
    (format t "~3d" (* (+ 1 x) (+ 1 y))))
  (format t "~%"))

(loop for i from 1 to 10 summing (expt i 2))

(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

(do-primes (p 0 19)
  (format t "~d " p))

(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p))

(defmacro do-primes ((var start end) &body body)
      `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
	 ((> ,var ,end))
	 ,@body))

(do-primes (p 0 190) (format t "~d " p))

(macroexpand-1 '(do-primes (p 0 190) (format t "~d " p))) 

(let ((in (open "~/code/lisp/t1.lisp" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
	 while line do (format t "~a~%" line))
    (close in)))

(let ((in (open "~/code/lisp/t1.lisp" :if-does-not-exist nil)))
  (when in
    (format t "~a~%" (read-line in))
    (close in)))

(defparameter *s* (open "~/code/lisp/t1.lisp"))

(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
	  lst
	  (our-member obj (cdr lst)))))

(defun askem (string)
  (format t "~a" string)
  (let ((val (read)))
    (if (numberp val)
	val
	(askem string))))

(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~a ~a~%" i (* i i))))

(defun our-reverce (lst)
  (let ((acc nil))
    (dolist (elt lst)
      (push elt acc))
    acc))

(defun bin-search (obj vec)
  (let ((len (length vec)))
    (and (not (zerop len))
	 (finder obj vec 0 (- len 1)))))

(defun finder (obj vec start end)
  (format t "~a~%" (subseq vec start (+ end 1)))
  (let ((range (- end start)))
    (if (zerop range)
	(if (eql obj (aref vec start))
	    obj
	    nil)
	(let ((mid (+ start (round (/ range 2)))))
	  (let ((obj2 (aref vec mid)))
	    (if (< obj obj2)
		(finder obj vec start (- mid 1))
		(if (> obj obj2)
		    (finder obj vec (+ mid 1) end)
		    obj)))))))

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
	 (do ((forward 0 (+ forward 1))
	      (back (- len 1) (- back 1)))
	     ((or (> forward back)
		  (not (eql (elt s forward)
			    (elt s back))))
	      (> forward back))))))

(defun our-member (obj lst)
  (if (atom lst)
      nil
      (if (eql (car lst) obj)
	  lst
	  (our-member obj (cdr lst)))))

(defun month-length (mon)
  (case mon
    ((jan mar may jul aug oct dec) 31)
    ((apr jun sept nov) 30)
    (feb (if (leap-year) 29 28))
    (otherwise "unknown month")))
