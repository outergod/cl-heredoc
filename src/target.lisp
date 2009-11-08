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

;;; helpers
(defun cl-forms (list target environment)
  "cl-forms list target environment => list

Return new list with each element of input list interpolated and prepended with
an additional \"--eval\" element string."
  (let ((forms (list)))
    (dolist (form list forms)
      (setq forms
            (nconc forms (list "--eval"
                               (interpolate-argument
                                (if (listp form)
                                    (format nil "(~a)" (deflate-string form))
                                  form)
                                target #'default-sourcefn environment)))))))


(defun cl-load-ops (list)
  "cl-load-ops list => list

Prepares a list of asdf:load-op forms for op in input list."
  (mapcar #'(lambda (elt)
              `(asdf:oos 'asdf:load-op ,elt))
          list))


(defmacro with-slot-enhanced-environment ((slots object) &body body)
  "with-slot-enhanced-environment (slots object) body => context

Create lexical context overriding *environment* with a fresh copy enhanced by
all slot names/values as key/values from symbol list slots in object."
  `(let ((*environment* (alexandria:copy-hash-table *environment*)))
    (mapc #'(lambda (slot)
              (setf (gethash slot *environment*)
                    (write-to-string (slot-value ,object slot))))
          ,slots)
   ,@body))


;;; evolvable, base target class
(defclass evolvable ()
  ((name :reader   name
         :initarg  :name
         :initform (alexandria:required-argument :name))
   (dependencies :reader   dependencies
                 :initarg  :dependencies
                 :initform nil)
   (env-slots :accessor env-slots
              :initarg  :env-slots
              :initform nil))
  (:documentation "Base class for all evolvables.

[slots]
name (string):
  The name of the evolvable, also available in evol's environment
dependencies (list):
  List of evolvables this one depends on
env-slots (list):
  List of slots to lexically bind to the environment during an evolvable's
  evolution"))

(defmethod initialize-instance :after ((evol evolvable) &rest initargs)
  "Also register evolvable in the evol *environment*"
  (declare (ignore initargs))
  (setf (gethash (internify (name evol)) *environment*) evol))

(defgeneric evolve (evolvable)
  (:documentation "Evolve this, whatever that may be"))

(defmethod evolve :around ((evol evolvable))
  (with-slot-enhanced-environment ((env-slots evol) evol)
                                  (call-next-method)))

; (defmethod evolve :before ((evol evolvable))
; TODO dependencies  


;;; virtual class
(defclass virtual (evolvable) ()
  (:documentation "Virtual evolvables exist for the sole purpose of
beautification through grouping and/or naming by having its dependencies
evolve."))

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
sources.

[slots]
rule (mixed):
  The rule used to evolve the definite
sourcefn (fn):
  The function to compute the input from other slots like e.g. target and
  name"))


;;; generic-transformator class
(defclass generic-transformator (definite)
  ((rule :accessor rule
         :initarg :rule
         :initform (alexandria:required-argument :rule)))
  (:documentation "Objects of this kind evolve through running an external
program through interpolating the rule and source function contained within
honoring common quoting rules in line with Bourne shell syntax."))

(defmethod evolve ((trans generic-transformator))
  (run-command
   (interpolate-commandline (rule trans))
   :target (name trans) :sourcefn (sourcefn trans)))


;;; cl-transformator class
(defclass cl-transformator (definite)
  ((rule :accessor rule
         :initarg :rule
         :initform (alexandria:required-argument :rule)))
  (:documentation "Evolution takes place here through running a freshly forked
Common Lisp copy that expects rule to be a list of forms to execute in order.
sourcefn is expected to return list of valid Common Lisp forms that will each be
grouped as a single argument to be passed to (eval) so no special quoting aside
from \\\" is required.
Variable expansion is only performed against sourcefn's return forms."))

(defmethod evolve ((trans cl-transformator))
  (run-command
   (nconc (split-commandline "sbcl --noinform --disable-debugger")
          (cl-forms
           (funcall (sourcefn trans) (rule trans))
           (name trans) *environment*))))


;;; checkable class
(defclass checkable (evolvable) ()
   (:documentation "Evolvables derived from checkable provide a means to pre- and
post-validate their evolution."))

(defgeneric evolved-p (checkable)
  (:documentation "Check that given evolution has been evolved properly"))

(defmethod evolve :around ((evol checkable))
  (or (evolved-p evol)
      (call-next-method)))


;;; file class
(defclass file (checkable) ()
  (:documentation "Files are targets that usually lead to evolution
of... files. Their existence can easily be checked through their distinct
pathnames."))

(defmethod evolved-p ((file file))
  (file-exists-p (cl-fad:pathname-as-file (name file))))


;;; executable
(defclass executable (file) ()
  (:documentation "Executables are files that can be run on a machine's stack by
either containing machince code themselves or referring to an interpreter for
source code contained within. This class ensures its file is executable after
creation."))

(defmethod evolve :after ((exe executable))
  (run-command (interpolate-commandline "chmod +x %@" :target (name exe))))


;;; cl-core class
(defclass cl-core (cl-transformator file)
  ((sourcefn :initform #'(lambda (rule)
                           `((require 'asdf)
                             ,@(cl-load-ops rule)
                             (in-package %package)
                             (sb-ext:save-lisp-and-die "%@" :toplevel %toplevel
                                                            :purify %purify))))
   (init-package :accessor init-package
                 :initarg :init-package
                 :initform (alexandria:required-argument :init-package))
   (toplevel :accessor toplevel
             :initarg :toplevel
             :initform nil)
   (purify   :accessor purify
             :initarg :purify
             :initform t)
   (env-slots :initform (list 'init-package 'toplevel 'purify)))
  (:documentation "This evolvable enables creation of non-standalone Common Lisp
core files. Right now, only sbcl is supported.
Feed rule with a list of asdf-recognized package symbols to load into the
core.

[slots]
sourcefn (fn):
  Preinitialized for this class; returns a list of forms to first load asdf,
  then in turn additional asdf packages from rule and finally a form to have
  sbcl create a core file.
init-package (symbol):
  Package to change to. This is neccessary because package names cannot be
  quoted and furthermore without this, the toplevel function couldn't be defined
  proplery.
toplevel (symbol):
  Name of the function to initally load after core has been loaded itself;
  see http://www.sbcl.org/manual/Saving-a-Core-Image.html
purify (boolean):
  see http://www.sbcl.org/manual/Saving-a-Core-Image.html"))


;;; cl-exe class
(defclass cl-exe (cl-transformator executable)
  ((sourcefn :initform #'(lambda (rule)
                           `((require 'asdf)
                             ,@(cl-load-ops rule)
                             (in-package %init-package)
                             (sb-ext:save-lisp-and-die "%@" :executable t
                                                            :toplevel %toplevel
                                                            :purify %purify)))
             :reader sourcefn
             :allocation :class)
   (init-package :accessor init-package
                 :initarg :init-package
             :initform (alexandria:required-argument :init-package))
   (toplevel :accessor toplevel
             :initarg :toplevel
             :initform nil)
   (purify   :accessor purify
             :initarg :purify
             :initform t)
   (env-slots :initform (list 'init-package 'toplevel 'purify)))
  (:documentation "In line with cl-core, a complete dump is generated but with
the engine fully runable contained within so the resulting file is a real
executable.
Feed rule with a list of asdf-recognized package symbols to load into the
binary.

[slots]
sourcefn (fn):
  Preinitialized for this class; returns a list of forms to first load asdf,
  then in turn additional asdf packages from rule and finally a form to have
  sbcl create the executable.
init-package (symbol):
  Package to change to. This is neccessary because package names cannot be
  quoted and furthermore without this, the toplevel function couldn't be defined
  proplery.
toplevel (symbol):
  Name of the function to initally load after core has been loaded itself;
  see http://www.sbcl.org/manual/Saving-a-Core-Image.html
purify (boolean):
  see http://www.sbcl.org/manual/Saving-a-Core-Image.html"))
