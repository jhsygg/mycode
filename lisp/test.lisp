(defun hello-world ()
  (format t "hello, world"))

;;3.Practical: A simple Database

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(make-cd "Roses" "Kahy Mattea" 7 t)

(defvar *db* nil)

(defun add-record (cd)
  (push cd *db*))

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))

(add-record (make-cd "Fly" "Dixie Chicks" 8 t))

(add-record (make-cd "Home" "Dixie Chicks" 9 t))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a: ~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(save-db "~/code/lisp/my-cds.db")

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(load-db "~/code/lisp/my-cds.db")

;;(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))

;;(remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))

;;(defun select-by-artist (artist)
;;  (remove-if-not
;;   #'(lambda (cd) (equal (getf cd :artist) artist))
;;   *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;(select #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")))

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

;;(select (where :artist "DiXie Chicks"))
;;(select (where :rating 10 :ripped nil))

;;(defun where (&key title artist rating (ripped nil ripped-p))
;;  #'(lambda (cd)
;;      (and 
;;      (if title (equal (getf cd :title) title) t)
;;      (if artist (equal (getf cd :artist) artist) t)
;;      (if rating (equal (getf cd :rating) rating) t)
;;      (if ripped-p (equal (getf cd :rapped) ripped) t))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title (setf (getf row :title) title))
	       (if artist (setf (getf row :artist) artist))
	       (if rating (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     row) *db*)))

;;(update (where :artist "Dixie Chicks" :rating 11))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

;;(select (where :title "Give Us a Break" :repped t))
;;编写一个代码转换预处理器(macro)，将上面的语句将转换为下面的语句。
;;(select
;; #'(lambda (cd)
;;    (and (equal (getf cd :title) "Give Us a Break")
;;         (equal (getf cd :ripped) t))))

;;(reverse '(1 2 3 4))

;;(defmacro backwords (expr) (reverse expr))

;;(backwords ("hello, world" t format))
;;执行后生成(format t "hello, world")
;;再执行(format t "hello, world")产生输出hello, world

;;消除where中的重复代码
;;(equal (getf cd field) value)

;;(defun make-comparison-expr (field value)
;;  (list 'equal (list 'getf 'cd field) value))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparison-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparison-list clauses))))
