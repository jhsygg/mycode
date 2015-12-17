(defun tokens (str fn start)
  (let ((p1 (position-if fn str :start start))) ;p1是第一个满足fn要求的字符位置
    (if p1
	;;p2是第一个不满足fn要求的字符位置
	(let ((p2 (position-if #'(lambda (x) (not (funcall fn x))) str :start p1)))
	  (cons (subseq str p1 p2)
		(if p2 (tokens str fn p2)))))))

(defun visible-char-p (c)
  (and (graphic-char-p c)
       (not (char= #\space c))))

(defun parse-date (str)
  (let ((tks (tokens str #'visible-char-p 0)))
    (list (parse-integer (car tks))
	  (parse-month (cadr tks)) (parse-integer (third tks)))))

(defconstant months
  '(("jan" . '一月) ("feb" . '二月) ("mar" . '三月) ("apr" . '四月) ("may" . '五月) ("jun" . '六月) ("jul" . 七月) ("aug" . '八月) ("sep" . '九月) ("oct" . '十月) ("nov" . '十一月) ("dec" . '十二月)))

(defun parse-month (str)
  (let ((p (assoc str months :test #'string-equal)))
    (if p (cdr p))))
;;递归数表中是否含有奇数
(defun anyoddp (x)
  (cond ((null x) nil)
	((oddp (first x)) t)
	(t (anyoddp (rest x)))))

;;返回数列中第一个奇数
(defun find-first-odd (x)
  (cond ((null x) nil)
	((oddp (first x)) (first x))
	(t (find-first-odd (rest x)))))

;;递归返回列表中最后一个元素
(defun last-element (x)
  (cond ((null (cdrx)) (first x))
	(t (last-element (rest x)))))

;;递归累加
(defun add-nums (n)
  (cond ((zerop n) 0)
	(t (+ n (add-nums (- n 1))))))

(defun anyoddp-if (x)
  (if (null x)
      nil
      (if (oddp (first x))
	  t
	  (anyoddp (rest x)))))
(defun laugh (n)
  (cond ((equal n 0) nil)
	(t (cons 'ha (laugh (- n 1))))))

(defun add-up (x)
  (cond ((null x) 0)
	(t (+ (first x) (add-up (rest x))))))

(defun fib (n)
  (cond ((equal n 0) 1)
	((equal n 1) 1)
	(t (+ (fib (- n 1)) (fib (- n 2))))))

(defun fact (n)
  (cond ((zerop n) 1)
	(t (* n (fact (- n 1))))))

;;全是奇数
(defun alloddp (x)
  (cond ((null x) t)
	((evenp (first x)) nil)
	(t (alloddp (rest x)))))

;;member的递归版本
(defun my-member (x y)
  (cond ((null y) nil)
	((= x (first y)) y)
	(t (my-member x (rest y)))))

;;递归的assoc
(defun rec-assoc (x y)
  (cond ((member x (car y)) (first y))
	(t (rec-assoc x (rest y)))))
;;递归的nth
(defun rec-nth (n x)
  (cond ((zerop n) (car x))
	(t (rec-nth (- n 1) (cdr x)))))

(defun find-number (x)
  (cond ((numberp x) x)
	((atom x) nil)
	(t (or (find-number (car x))
	       (find-number (cdr x))))))

(defun atoms-to-q (x)
  (cond ((null x) nil)
	((atom x) 'q)
	(t (cons (atoms-to-q (car x))
		 (atoms-to-q (cdr x))))))

(defmacro simple-incf (var)
  (list 'setq var (list '+ var 1)))
;;存在变量捕获问题
(defmacro ntimes (n &rest body)
  `(do ((x 0 (+ x 1)))
       ((>= x ,n))
     ,@body))
;;修改后
(defmacro ntimes (n &rest body)
  (let ((a (gensym)))
    `(do ((,a 0 (+ ,a 1)))
	 ((>= ,a ,n))
       ,@body)))
