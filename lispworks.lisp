
(in-package :common-lisp-user)

;;; Only use standard nicknames for the COMMON-LISP and
;;; COMMON-LISP-USER packages
(rename-package "COMMON-LISP-USER" "COMMON-LISP-USER" '("CL-USER"))
(rename-package "COMMON-LISP" "COMMON-LISP" '("CL"))

;;; LispWorks personal customizations

(defvar *lw-custom-directory*
  (merge-pathnames
   (make-pathname
    :directory '(:relative "common-lisp" "LispWorks"))
   (user-homedir-pathname))
  "Location of personal customizations to LispWorks.")

;;; Editor customizations

(load (merge-pathnames "editor" *lw-custom-directory*))

;;; TMH-USER

(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "common-lisp" "tmh-user")
   :name "tmh-user")
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
