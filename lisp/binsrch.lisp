(defun finder (obj vec left right)
  (let ((range (- right left)))
    (if (= 0 range)
	;;处理只有一个元素的情形
	(if (equal obj (aref vec left)) obj nil)
	(let* ((mid (+ left (round (/ range 2)))) (obj1 (aref vec mid)))
	  (if (< obj obj1)
	      (finder obj vec left (1- mid)) ;在数组的前半部分
	      (if (> obj obj1)
		  (finder obj vec (1+ mid) right) ;在数组的后半部分
		  obj))))))

(defun bin-search (obj vec)
  (let ((len (length vec)))
    (and (not (zerop len))
	 (finder obj vec 0 (1- len)))))
	
