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
