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

(defclass target ()
  ((name :accessor target-name
         :initarg :name)
   (dependencies :accessor target-dependencies
                 :initarg dependencies
                 :initform (list))))

(defmethod initialize-instance :after ((target target) &rest initargs)
  (declare (ignore initargs))
  (setf (gethash (internify (target-name target)) *environment*) target))


(defclass evolvable (target)
  ((rule :accessor rule
         :initarg :rule)
   (sourcefn :accessor sourcefn
             :initarg :sourcefn
             :initform #'default-sourcefn)))

(defgeneric evolve (evolvable))
; (defmethod evolve :before ((evolvable evolvable))
; TODO dependencies  


(defclass checkable (target)
  ((check :accessor check
          :initarg :check)))

(defmethod check-exists ((checkable checkable)) nil)


(defclass evolve-checkable (evolvable checkable) ())
(defmethod evolve :around ((ef evolve-checkable))
  (when (not (check-exists ef))
    (call-next-method))
  (check-exists ef))


(defclass file (evolve-checkable) ())

(defmethod evolve ((file file))
  (run-command (rule file) :target (target-name file) :sourcefn (sourcefn file)))

(defmethod check-exists ((file file))
  (file-exists-p (cl-fad:pathname-as-file (target-name file))))


(defmacro deftarget (name &key (actionfn #'(lambda ())) (dependencies nil))
  `(setf (gethash ',name *targets*)
         (defun ,name ()
           (mapc #'funcall ,dependencies)
           (funcall ,actionfn))))
