
(in-package :common-lisp-user)

;;; TMH-USER

(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "common-lisp" "tmh-user")
   :name "tmh-user"
   :type "lisp")
  (user-homedir-pathname)))

;;; Quicklisp

#-quicklisp
(let ((quicklisp-init
       (merge-pathnames
        (make-pathname
         :directory '(:relative "quicklisp")
         :name "setup"
         :type "lisp")
        (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; Commonly used libraries

#+quicklisp
(progn
  (quicklisp:quickload "meta-sexp")
  (quicklisp:quickload "lisp-unit"))
