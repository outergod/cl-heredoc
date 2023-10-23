;;;; cl-heredoc - heredoc.lisp
;;;; Copyright (C) 2010  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of cl-heredoc.
;;;; cl-heredoc is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; cl-heredoc is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-heredoc)

(defun read-until-match (stream terminal)
  "read-until-match stream terminal => string

Read characters from STREAM until a sequence equal to string TERMINAL is read.
Return all characters read as string omitting TERMINAL itself. Signal error upon
EOF."
  (with-output-to-string (out)
    (do* ((match-length (length terminal))
          (buffer (new-ring-buffer match-length))
          (buffer-char nil)
          (char (read-char stream t :eof t)
                (or (setf buffer-char (ring-buffer-next buffer))
                    (read-char stream t :eof t)))
          (match-pos 0))
        ((eql char :eof))
      (cond ((char= char (char terminal match-pos))
             (when (= (incf match-pos) match-length)
               (return))
             (unless buffer-char 
               (ring-buffer-insert buffer char)))
            ((zerop match-pos)
             (write-char char out)
             (when buffer-char
               (ring-buffer-pop buffer)))
            (t
             (unless buffer-char
               (ring-buffer-insert buffer char))
             (write-char (ring-buffer-pop buffer) out)
             (setf match-pos 0))))))

(defun read-heredoc (stream char arg)
  "read-heredoc stream char arg => string

Return string from STREAM up to the point where the string read first until CHAR
is encountered. All evaluation is completely turned off so no quoting is
required at all.
Example:
CL-USER> (set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)
CL-USER> #>eof>Write whatever (you) \"want\"!eof => Write whatever (you) \"want\"!"
  (declare (ignore arg))
  (read-until-match stream (read-until-match stream (string char))))

(defvar *previous-readtables* nil
  "A stack which holds the previous readtables that have been pushed
here by ENABLE-HEREDOC-SYNTAX.")

(defun %enable-heredoc-syntax ()
  "Internal function used to enable reader syntax and store current
readtable on stack."
  (push *readtable*
        *previous-readtables*)
  (setq *readtable* (copy-readtable))
  (set-dispatch-macro-character #\# #\> #'read-heredoc)
  (values))

(defun %disable-heredoc-syntax ()
  "Internal function used to restore previous readtable." 
  (if *previous-readtables*
    (setq *readtable* (pop *previous-readtables*))
    (setq *readtable* (copy-readtable nil)))
  (values))

(defmacro enable-heredoc-syntax ()
  "Enable CL-HEREDOC reader syntax."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-heredoc-syntax)))

(defmacro disable-heredoc-syntax ()
  "Restore readtable which was active before last call to
ENABLE-HEREDOC-SYNTAX. If there was no such call, the standard
readtable is used."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%disable-heredoc-syntax)))
