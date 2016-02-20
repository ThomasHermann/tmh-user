#|

 TMH-USER

 Copyright (c) 2009-2016, Thomas M. Hermann

 Permission is hereby granted, free  of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction,  including without limitation the rights
 to use, copy, modify,  merge,  publish,  distribute,  sublicense, and/or sell
 copies of the  Software,  and  to  permit  persons  to  whom  the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and  this  permission  notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED  "AS IS",  WITHOUT  WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT  NOT  LIMITED  TO  THE  WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE  AND  NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT  HOLDERS  BE  LIABLE  FOR  ANY  CLAIM,  DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.

|#

;;; Some pointless utilities to mold the REPL to my liking.

(in-package :common-lisp-user)

(defpackage :tmh-user
  (:use :common-lisp :common-lisp-user)
  #+sbcl
  (:use :sb-ext)
  #+ccl
  (:use :ccl)
  #+lispworks
  (:use :lispworks :hcl)
  ;; Symbols
  (:export :funbind :sunbind
           :list-external-symbols)
  ;; Pathnames
  (:export :probe-pathname
           :user-directory
           :map-file-to-string)
  ;; List functions
  (:export :cartesian-product
           :nary-product
           :permutations)
  ;; Magic 8 ball
  (:export :magic-8-ball))

(in-package :tmh-user)

;;; Symbol utilities

(defun funbind (name)
  "Remove the function or macro definition, providing a continuable
error if not bound."
  (assert (fboundp name)
          (name)
          "~S is not bound to a function or macro." name)
  (fmakunbound name)
  (unless (boundp name)
    (unintern name)))

(defun sunbind (name)
  "Remove the symbol definition, providing a continuable error if not
bound."
  (assert (boundp name)
          (name)
          "~S is not bound to a symbol." name)
  (makunbound name)
  (unless (fboundp name)
    (unintern name)))

(defun list-external-symbols (&optional (package *package*))
  "Return a list of the external symbols in the package."
  (loop for extsym being each external-symbol in package
        collect extsym))

;;; Pathname & files

(defun probe-pathname (&rest all-keys &key
                       host device directory name type
                       version defaults case)
  "Make and probe the pathname."
  (declare (ignore host device directory name type))
  (declare (ignore version defaults case))
  (probe-file (apply #'make-pathname all-keys)))

(defun user-directory (&rest path-elements)
  "Return a directory relative to USER-HOMEDIR-PATHNAME."
  (let ((directory (merge-pathnames
                    (make-pathname
                     :directory (list* :relative path-elements))
                    (user-homedir-pathname))))
    (assert (probe-file directory)
        (directory)
      "~A is not a directory." directory)
    directory))

(defun naggum-map-file-to-string (pathname)
  "Create a string that contains all the characters of a file.

Erik Naggum : 1998-04-15"
  ;;this should have used a memory mapping function
  (with-open-file (file pathname :direction :input)
    (let ((string (make-array (file-length file)
                              :element-type (stream-element-type file)
                              #+allegro :allocation #+allegro :old)))
      (if (= (length string) (read-sequence string file))
          string
          (error 'file-error
                 :pathname pathname
                 :format-control "~@<~S could not be mapped to a string.~:@>"
                 :format-arguments (list pathname))))))

(defun map-file-to-string (pathname &optional (buffer-size 8192))
  "Create a string that contains all the characters of a file."
  (with-open-file (file pathname :direction :input)
    (let ((size (file-length file)))
      (if (< size array-total-size-limit)
          (with-output-to-string (out)
            (loop with buffer = (make-string buffer-size)
                  for pos = (read-sequence buffer file)
                  as read-count = pos then (+ read-count pos)
                  do (write-string buffer out :end pos)
                  until (or (< size read-count) (zerop pos))))
          (error "The file size exceeds ARRAY-TOTAL-SIZE-LIMIT.")))))

;;; Magic 8-Ball

(defun magic-8-ball (query)
  "Seek the advice of the Magic 8-ball."
  (declare (ignore query))
  (svref #("As I see it, yes"
           "It is certain"
           "It is decidedly so"
           "Most likely"
           "Outlook good"
           "Signs point to yes"
           "Without a doubt"
           "Yes" "Yes - definitely"
           "Reply hazy, try again"
           "Ask again later"
           "Better not tell you now"
           "Cannot predict now"
           "Concentrate and ask again"
           "Don't count on it"
           "My reply is no"
           "My sources say no"
           "Outlook not so good"
           "Very doubtful")
          (random 19)))

;;; List functions

(defun cartesian-product (list1 list2)
  "Return a list of the Cartesian product of two lists."
  (mapcan (lambda (x) (mapcar (lambda (y) (list x y)) list2)) list1))

(defun nary-product (list1 list2 &rest more-lists)
  "Return a list of the n-ary Cartesian product of the lists."
  (if (null more-lists)
      (cartesian-product list1 list2)
      (mapcan
       (lambda (x)
         (mapcar (lambda (y) (push x y))
                 (apply #'nary-product list2
                        (car more-lists) (rest more-lists))))
       list1)))

(defun permutations (list)
  "Return permutations of the list. [Erik Naggum]"
  (if (cdr list)
      (loop
       with rotation = list
       do (setq rotation (nconc (last rotation) (nbutlast rotation)))
       nconc
       (loop
        for list in (permutations (rest rotation))
        collect (cons (first rotation) (copy-list list)))
       until (eq rotation list))
      (list list)))

(defun factorial (n)
  (if (minusp n)
      (error "Argument must be positive.")
      (if (zerop n) 1 (* n (factorial (1- n))))))

(defun combinations (nelm kcomb)
  (/ (factorial nelm)
     (factorial kcomb)
     (factorial (- nelm kcomb))))
