(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record (cd) (push cd *db*))

;;(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
;;(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
;;(add-record (make-cd "Home" "Dixie Chicks" 9 t))
;; 
;;(defun dump-db ()
;;(dolist (cd *db*)
;;  (format t "~{~a: ~10t~a~%~}~%" cd)))

(defun dump-db ()
  (format t "~{~{~a: ~10t~a~%~}~%~}" *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;;(defun prompt-for-cd ()
;;  (make-cd
;;   (prompt-read "Title")
;;   (prompt-read "Artist")
;;   (prompt-read "Rating")
;;   (y-or-n-p "Ripped [y/n]: ")))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

;;y-or-n-p 函数，会在没有输入y,Y,n,N 开始的内容时重新提示输入。
;;or 是宏 “或”
;;parse-integer 从字符中取整数

;;上语句形成了强壮的输入。

;;最后将prompt-for-cd包装在一个不停循环直到用户完成的函数里，以RETURN来退出。

;;==================
;;loop宏
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

;;with-open-file宏会打开一个文件，将文件流绑定到一个变量上，执行一组表达式，然后再关闭这个文件。
;;宏with-standard-io-syntax确保那些影响PRINT行为的特定变量可以被设置成它们的标准值。
(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;;重载数据库
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;;==================
;; 查询数据库

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title     (equal (getf cd :title) title) t)
       (if artist    (equal (getf cd :artist) artist) t)
       (if rating    (equal (getf cd :rating) rating) t)
       (if ripped-p  (equal (getf cd :ripped) ripped) t))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title    (setf (getf row :title) title))
	       (if artist   (setf (getf row :artist) artist))
	       (if rating   (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))


