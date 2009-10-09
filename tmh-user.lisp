#|

 TMH-USER

 Copyright (c) 2009, Thomas M. Hermann
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

(common-lisp:defpackage #:tmh-user
  (:use #:common-lisp #:common-lisp-user
        #+sbcl #:sb-ext
        #+ccl  #:ccl)
  (:export #:funbind
           #:sunbind
           #:magic-8-ball))

(common-lisp:in-package #:tmh-user)

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

(defun magic-8-ball (query)
  "Seek the advice of the Magic 8-ball."
  (declare (ignore query))
  (svref
   #("As I see it, yes" "It is certain"
     "It is decidedly so" "Most likely"
     "Outlook good" "Signs point to yes"
     "Without a doubt" "Yes" "Yes - definitely"
     "Reply hazy, try again" "Ask again later"
     "Better not tell you now" "Cannot predict now"
     "Concentrate and ask again" "Don't count on it"
     "My reply is no" "My sources say no"
     "Outlook not so good" "Very doubtful")
   ;; Shake the Magic 8-Ball
   (loop
    with counter = -1
    with vec = (map-into (make-array 20)
                         (lambda () (incf counter)))
    for index below 20
    and swap = (random 20) do
    (rotatef (svref vec index) (svref vec swap))
    finally (return (svref vec (random 20))))))

