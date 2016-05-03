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
(defvar *oclapi-helper-error* t)

(defun api-error (name code)
  (when *oclapi-helper-error*
    (error "~s error ~s" name code)))

@export
(defmacro when-success (expr &body body)
  `(when (= +cl-success+ ,expr)
     ,@body))

@export
(defun get-platform-id ()
  (with-foreign-objects ((platforms 'cl-platform-id)
                         (num-platforms 'cl-uint))
    (when-success (cl-get-platform-ids 1 platforms num-platforms)
      (mem-aref platforms 'cl-platform-id))))

@export
(defun get-platform-ids ()
  (with-foreign-object (num-platforms 'cl-uint)
    (when-success (cl-get-platform-ids 0 (null-pointer) num-platforms)
      (let ((num (mem-aref num-platforms 'cl-uint)))
        (with-foreign-object (platforms 'cl-platform-id num)
          (when-success (cl-get-platform-ids num platforms num-platforms)
            (loop for n from 0 below num
                  collecting (mem-aref platforms 'cl-platform-id n))))))))

@export
(defun create-buffer (context flags size &optional (host-ptr (null-pointer)))
  (with-foreign-object (errcode-ret 'cl-int)
    (let ((buffer (cl-create-buffer context
                                    flags
                                    size
                                    host-ptr
                                    errcode-ret)))
      (if (= +cl-success+ (mem-aref errcode-ret 'cl-int))
          buffer
          (api-error 'cl-create-buffer
                     (mem-aref errcode-ret 'cl-int))))))
