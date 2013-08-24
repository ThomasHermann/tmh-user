
(in-package :common-lisp-user)

;;; Only use standard nicknames for the COMMON-LISP and
;;; COMMON-LISP-USER packages
(rename-package "COMMON-LISP-USER" "COMMON-LISP-USER" '("CL-USER"))
(rename-package "COMMON-LISP" "COMMON-LISP" '("CL"))

;; Rely on outside versions of ASDF and integration
(setf lispworks:*autoload-asdf-integration* nil)

;;; LispWorks personal customizations

(defvar *lw-custom-directory*
  (merge-pathnames
   (make-pathname
    :directory '(:relative "Lisp" "LispWorks"))
   (user-homedir-pathname))
  "Location of personal customizations to LispWorks.")

;;; Editor customizations

(load (merge-pathnames "editor" *lw-custom-directory*))

;;; TMH-USER

(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Lisp" "tmh-user")
   :name "tmh-user")
  (user-homedir-pathname)))

;;; Quicklisp

#-quicklisp
(let ((quicklisp-init
       (merge-pathnames
        (make-pathname
         :directory '(:relative "Quicklisp")
         :name "setup"
         :type "lisp")
        (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)
    (provide "asdf")))

;;; ASDF Integration

#+asdf
(let ((*compile-verbose* nil)
      (*compile-print*   nil)
      (asdf-integration
       (merge-pathnames
        (make-pathname :name "asdf-integration")
        *lw-custom-directory*)))
  ;; ASDF integration with LispWorks
  (handler-case (compile-file-if-needed asdf-integration :load t)
    (conditions:fasl-error ()
      (load (compile-file asdf-integration)))))

;;; Commonly used libraries

#+quicklisp
(progn
  (quicklisp:quickload "meta-sexp")
  (quicklisp:quickload "lisp-unit"))
