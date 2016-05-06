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

(defun check-errcode-ret (result name errcode-ret)
  (let ((code (mem-aref errcode-ret 'cl-int)))
    (if (= +cl-success+ code)
        result
        (api-error name code))))

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
(defun create-context (properties
                       num-devices
                       devices
                       &optional
                         (pfn-notify (null-pointer))
                         (user-data (null-pointer)))
  (with-foreign-object (errcode-ret 'cl-int)
    (let ((context (cl-create-context properties
                                      num-devices
                                      devices
                                      pfn-notify
                                      user-data
                                      errcode-ret)))
      (check-errcode-ret context 'cl-create-context errcode-ret))))

@export
(defun create-buffer (context flags size &optional (host-ptr (null-pointer)))
  (with-foreign-object (errcode-ret 'cl-int)
    (let ((buffer (cl-create-buffer context
                                    flags
                                    size
                                    host-ptr
                                    errcode-ret)))
      (check-errcode-ret buffer 'cl-create-buffer errcode-ret))))

@export
(defun create-program-with-source (context count strings &optional (lengths (null-pointer)))
  (with-foreign-object (errcode-ret 'cl-int)
    (let ((program (cl-create-program-with-source context
                                                  count
                                                  strings
                                                  lengths
                                                  errcode-ret)))
      (check-errcode-ret program 'cl-create-program-with-source errcode-ret))))

@export
(defun create-kernel (program kernel-name)
  (with-foreign-object (errcode-ret 'cl-int)
    (let ((kernel (cl-create-kernel program kernel-name errcode-ret)))
      (check-errcode-ret kernel 'cl-create-kernel errcode-ret))))

@export
(defun create-command-queue (context device properties)
  (with-foreign-object (errcode-ret 'cl-int)
    (let ((command-queue (cl-create-command-queue context
                                                  device
                                                  properties
                                                  errcode-ret)))
      (check-errcode-ret command-queue 'cl-create-command-queue errcode-ret))))

@export
(defun enqueue-ndrange-kernel (command-queue
                               kernel
                               work-dim
                               global-work-offset
                               global-work-size
                               local-work-size
                               &optional
                                 (num-events-in-wait-list 0)
                                 (event-wait-list (null-pointer))
                                 (event (null-pointer)))
  (let ((result (cl-enqueue-ndrange-kernel command-queue
                                           kernel
                                           work-dim
                                           global-work-offset
                                           global-work-size
                                           local-work-size
                                           num-events-in-wait-list
                                           event-wait-list
                                           event)))
    (unless (= +cl-success+ result)
      (api-error 'enqueue-ndrange-kernel result))))

@export
(defun set-platform-id (properties platform-id)
  (setf (mem-aref properties 'cl-context-properties 0) +cl-context-platform+)
  (setf (mem-aref properties 'cl-platform-id 1) platform-id)
  (setf (mem-aref properties 'cl-context-properties 2) 0))

@export
(defun set-work-size (work-size x &optional (y 0) (z 0))
  (setf (mem-aref work-size 'size-t 0) x
        (mem-aref work-size 'size-t 1) y
        (mem-aref work-size 'size-t 2) z))
