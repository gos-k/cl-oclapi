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

@export
(defun print-foreign-array (foreign-array size type &key (step 1) (index nil))
  (loop for i from 0 below size by step
        do (let ((value (mem-aref foreign-array type i)))
             (if index
                 (format t "~a, ~a~%" i value)
                 (format t "~a~%" value)))))

@export
(defun device-memory-to-simple-array (command-queue device size type &key (step 1))
  (let (result)
    (with-foreign-objects ((foreign-array type size))
      (enqueue-read-buffer command-queue
                           device
                           +cl-true+
                           0
                           (* (foreign-type-size type) size)
                           foreign-array)
      (setf result (foreign-array-to-simple-array foreign-array size type :step step)))
    result))

@export
(defun device-memory-to-list (command-queue device size type &key (step 1))
  (let (result)
    (with-foreign-objects ((foreign-array type size))
      (enqueue-read-buffer command-queue
                           device
                           +cl-true+
                           0
                           (* (foreign-type-size type) size)
                           foreign-array)
      (setf result (foreign-array-to-list foreign-array size type :step step)))
    result))

@export
(defun print-device-memory (command-queue device size type &key (step 1) (index nil))
  (with-foreign-objects ((foreign-array type size))
    (enqueue-read-buffer command-queue
                         device
                         +cl-true+
                         0
                         (* (foreign-type-size type) size)
                         foreign-array)
    (print-foreign-array foreign-array size type :step step :index index)))
