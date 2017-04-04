;; program:mynetwork_bc_vs_1.0.0.lisp
;; author:futingfang
;;email:futingfang@163.com
;;
;;version:1.0.0(并、串行版)完整
;;date:2016-2-16
;;
;;info:run a common lisp script with sbcl
;;a neurons-net-database for test
;;
;;usage:
;;(load "mynetwork_bc_vs_1.0.0.lisp)
;;
;;(in-package neural-network-based)(main)





;;********************************************************************
;;********************************************************************

;;**************************系统各种结构、变量等的定义*************************
;;自定义包
(defpackage :neural-network-based
  (:use :common-lisp
	:common-lisp-user)
  (:export :save-db
	   :load-db))

;;
