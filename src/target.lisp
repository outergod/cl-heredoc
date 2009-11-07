;;;; evol - target.lisp
;;;; Copyright (C) 2009  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of evol.
;;;; evol is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; evol is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :evol)

;; (defparameter *targets* (make-hash-table))

;; (defun default-actionfn (target modifier)
;;   (declare (ignore target modifier)) "")

;;; evolvable, base target class
(defclass evolvable ()
  ((name :accessor name
         :initarg :name)
   (dependencies :accessor dependencies
                 :initarg dependencies
                 :initform nil))
  (:documentation "Base class for all evolvables"))

(defmethod initialize-instance :after ((evol evolvable) &rest initargs)
  "Also register evolvable in the evol *environment*"
  (declare (ignore initargs))
  (setf (gethash (internify (name evol)) *environment*) evol))

(defgeneric evolve (evolvable)
  (:documentation "Evolve this, whatever that may be"))

; (defmethod evolve :before ((evol evolvable))
; TODO dependencies  


;;; virtual class
(defclass virtual (evolvable) ()
  (:documentation "Virtual evolvables exist for the sole purpose of
beautification through grouping and/or naming by having its dependencies
evolve"))

(defmethod evolve ((virt virtual)) t)


;;; definite class
(defclass definite (evolvable)
  ((rule :accessor rule
         :initarg :rule)
   (sourcefn :accessor sourcefn
             :initarg :sourcefn
             :initform #'default-sourcefn))
  (:documentation "Definite evolvables define transformation rules and
computation of their effective input(s) to evolve, possibly from some kind of
sources"))


;;; checkable class
(defclass checkable (evolvable)
   ((check :accessor check
           :initarg :check))
   (:documentation "Evolvables derived from checkable provide a means to pre- and
post-validate their evolution."))

(defgeneric evolved-p (checkable)
  (:documentation "Check that given evolution has been evolved properly"))

(defmethod evolve :around ((evol checkable))
  (or (exists-p evol)
      (call-next-method)))


;;; file class
(defclass file (definite checkable) ()
  (:documentation "Files are targets that usually lead to evolution
of... files. Their existence can easily be checked through their distinct
pathnames."))

(defmethod exists-p ((file file))
  (file-exists-p (cl-fad:pathname-as-file (name file))))

(defmethod evolve ((file file))
  (run-command (rule file) :target (name file) :sourcefn (sourcefn file)))


;;; helpers
;; (defmacro deftarget (name &key (actionfn #'(lambda ())) (dependencies nil))
;;   `(setf (gethash ',name *targets*)
;;          (defun ,name ()
;;            (mapc #'funcall ,dependencies)
;;            (funcall ,actionfn))))
