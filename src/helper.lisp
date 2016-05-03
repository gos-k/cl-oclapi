(in-package :cl-user)
(defpackage cl-oclapi.helper
  (:use :cl
        :cffi
        :cl-annot
        :cl-oclapi.constants
        :cl-oclapi.types
        :cl-oclapi.functions))
(in-package :cl-oclapi.helper)

(annot:enable-annot-syntax)

@export
(defun get-platform-id ()
  (with-foreign-objects ((platforms 'cl-platform-id)
                         (num-platforms 'cl-uint))
    (let ((result (cl-get-platform-ids 1 platforms num-platforms)))
      (when (= +cl-success+ result)
          (mem-aref platforms 'cl-platform-id)))))

@export
(defun get-platform-ids ()
  (with-foreign-object (num-platforms 'cl-uint)
    (let ((result (cl-get-platform-ids 0 (null-pointer) num-platforms)))
      (when (= +cl-success+ result)
        (let ((num (mem-aref num-platforms 'cl-uint)))
          (with-foreign-object (platforms 'cl-platform-id num)
            (let ((result (cl-get-platform-ids num platforms num-platforms)))
              (when (= +cl-success+ result)
                (loop for n from 0 below num
                      collecting (mem-aref platforms 'cl-platform-id n))))))))))
