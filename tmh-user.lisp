#|

 TMH-USER

 Copyright (c) 2009-2011, Thomas M. Hermann
 All rights reserved.

 Redistribution and  use  in  source  and  binary  forms, with or without
 modification, are permitted  provided  that the following conditions are
 met:

   o  Redistributions of  source  code  must  retain  the above copyright
      notice, this list of conditions and the following disclaimer.
   o  Redistributions in binary  form  must reproduce the above copyright
      notice, this list of  conditions  and  the  following disclaimer in
      the  documentation  and/or   other   materials  provided  with  the
      distribution.
   o  The names of the contributors may not be used to endorse or promote
      products derived from this software without  specific prior written
      permission.

 THIS SOFTWARE IS  PROVIDED  BY  THE  COPYRIGHT  HOLDERS AND CONTRIBUTORS
 "AS IS"  AND  ANY  EXPRESS  OR  IMPLIED  WARRANTIES, INCLUDING,  BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES  OF MERCHANTABILITY AND FITNESS FOR A
 PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 EXEMPLARY, OR  CONSEQUENTIAL  DAMAGES  (INCLUDING,  BUT  NOT LIMITED TO,
 PROCUREMENT OF  SUBSTITUTE  GOODS  OR  SERVICES;  LOSS  OF USE, DATA, OR
 PROFITS; OR BUSINESS INTERRUPTION)  HOWEVER  CAUSED AND ON ANY THEORY OF
 LIABILITY, WHETHER  IN  CONTRACT,  STRICT  LIABILITY, OR TORT (INCLUDING
 NEGLIGENCE OR  OTHERWISE)  ARISING  IN  ANY  WAY  OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

|#

;;; Some utilities to mold the REPL to my liking.

(in-package :common-lisp-user)

(defpackage :tmh-user
  (:use :common-lisp :common-lisp-user
        #+sbcl :sb-ext
        #+ccl  :ccl
        #+lispworks :lispworks
        #+lispworks :hcl)
  (:export :funbind :sunbind
           :probe-pathname
           :user-directory
           :magic-8-ball
           ;; Units conversion
           :radians :degrees
           :inches :meters
           :psi :pascals))

(in-package :tmh-user)

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

(defun fas (name)
  "Abbreviation for FIND-ALL-SYMBOLS."
  (find-all-symbols (string name)))

;;; Pathname

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
   ;; Shake the Magic 8-Ball
   (loop with counter = -1
         with vec = (map-into (make-array 20)
                              (lambda () (incf counter)))
         for index below 20
         and swap = (random 20) do
         (rotatef (svref vec index) (svref vec swap))
         finally (return (svref vec (random 20))))))

;;; Units conversion : FIXME : Need a units library
(defun radians (angle-in-degrees)
  "Return the angle in radians."
  (* angle-in-degrees (/ pi 180D0)))

(defun degrees (angle-in-radians)
  "Return the angle in degrees."
  (* angle-in-radians (/ 180D0 pi)))

(defun inches (length-in-meters)
  "Return the length in inches."
  (/ length-in-meters 2.54D-2))

(defun meters (length-in-inches)
  "Return the length in meters."
  (* 2.54D-2 length-in-inches))

(defun pascals (pressure-in-psi)
  "Return the pressure in Pascals."
  (/ pressure-in-psi 1.45D-4))

(defun psi (pressure-in-pascals)
  "Return the pressure in PSI."
  (* 1.45D-4 pressure-in-pascals))
