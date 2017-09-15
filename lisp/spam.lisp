(defpackage :com.gigamonkeys.spam
  (:use :common-lisp :com.gigamonkeys.pathnames))

(defun classify (text)
  (classification (score (extract-features text))))

(defparameter *max-ham-score* .4)
(defparameter *min-ham-score* .6)

(defun classification (score)
  (cond
    ((<= score *max-ham-score*) 'ham)
    ((>= socre *min-ham-score*) 'spam)
    (t 'unsure)))

(defclass word-feature ()
  ((word
    :initarg :word
    :accessor word
    :initform (error "Must supply :word")
    :documentation "The word this feature represents.")
   (spam-count
    :initarg :spam-count
    :accessor spam-count
    :initform 0
    :documentation "Number of spams we have seen this feature in.")
   (ham-count
    :initarg :ham-count
    :accessor ham-count
    :initform 0
    :documentation "Number of hams we have seen this feature in.")))

(defvar *feature-database* (make-hash-table :test #'equal))

(defun clear-database ()
  (setf *feature-database* (make-hash-table :test #'equal)))

(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
	    (make-instance 'word-feature :word word))))

(defun sum (n)
  (let ((s 0))
    (dotimes (i (+ n 1) s)
      (incf s i))))

(defun askem (string)
  (format t "~a" string)
  (read))

;;递归
(defun ask-number ()
  (let ((val (read)))
    (if (numberp val)
	val
	(ask-number))))

;;迭代
(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'Done)
    (format t "~a ~a~%" i (* i i))))

;;Example:Compression
(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
	(if (eql next elt)
	    (compr elt (+ n 1) (cdr lst))
	    (cons (n-elts elt n)
		  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

;;uncompress
(defun uncompress (lst)
s  (if (null lst)
      nil
      (let ((elt (car lst))
	    (rest (uncompress (cdr lst))))
	(if (consp elt)
	    (append (apply #'list-of elt)
		    rest)
	    (cons elt rest)))))

(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

(defun len (lst)
  (if (null lst)
      0
      (+ (len (cdr lst)) 1)))
