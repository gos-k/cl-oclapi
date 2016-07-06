#|
  This file is a part of cl-oclapi project.
  Copyright (c) 2016 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-oclapi.helpers.memory
  (:use :cl
        :alexandria
        :cffi
        :cl-annot
        :cl-oclapi.types
        :cl-oclapi.constants
        :cl-oclapi.helpers.safe-call))
(in-package :cl-oclapi.helpers.memory)

(annot:enable-annot-syntax)

@export
(defun foreign-array-to-simple-array (foreign-array size type &key (step 1))
  (let ((array (make-array (floor (/ size step)))))
    (loop for i from 0 below size by step
          do (setf (aref array (floor (/ i step)))
                   (mem-aref foreign-array type i)))
    array))

@export
(defun foreign-array-to-list (foreign-array size type &key (step 1))
  (loop for i from 0 below size by step
        collecting (mem-aref foreign-array type i)))
