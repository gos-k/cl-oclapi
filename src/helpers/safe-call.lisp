#|
  This file is a part of cl-oclapi project.
  Copyright (c) 2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-oclapi.helpers.safe-call
  (:use :cl
        :cffi
        :cl-annot
        :cl-oclapi.constants
        :cl-oclapi.types
        :cl-oclapi.functions))
(in-package :cl-oclapi.helpers.safe-call)

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

(defmacro when-success (expr &body body)
  `(when (= +cl-success+ ,expr)
     ,@body))

(defmacro unless-success (expr &body body)
  `(unless (= +cl-success+ ,expr)
     ,@body))

(defun check-result (result name)
  (unless-success result
    (api-error name result)))

#| Platform APIs |#

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

#| Context APIs |#

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

#| Command Queue APIs |#

@export
(defun create-command-queue (context device properties)
  (with-foreign-object (errcode-ret 'cl-int)
    (let ((command-queue (cl-create-command-queue context
                                                  device
                                                  properties
                                                  errcode-ret)))
      (check-errcode-ret command-queue 'cl-create-command-queue errcode-ret))))

#| Memory Object APIs |#

@export
(defun create-buffer (context flags size &optional (host-ptr (null-pointer)))
  (with-foreign-object (errcode-ret 'cl-int)
    (let ((buffer (cl-create-buffer context
                                    flags
                                    size
                                    host-ptr
                                    errcode-ret)))
      (check-errcode-ret buffer 'cl-create-buffer errcode-ret))))

#| Program Object APIs  |#

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
(defun build-program (program
                      num-devices
                      device-list
                      &optional
                        (options (null-pointer))
                        (cl-callback (null-pointer))
                        (user-data (null-pointer)))
  (check-result (cl-build-program program
                                  num-devices
                                  device-list
                                  options
                                  cl-callback
                                  user-data)
                'build-program))

#| Kernel Object APIs |#

@export
(defun create-kernel (program kernel-name)
  (with-foreign-object (errcode-ret 'cl-int)
    (let ((kernel (cl-create-kernel program kernel-name errcode-ret)))
      (check-errcode-ret kernel 'cl-create-kernel errcode-ret))))

@export
(defun set-kernel-arg (kernel arg-index arg-size arg-value)
  (check-result (cl-set-kernel-arg kernel
                                   arg-index
                                   arg-size
                                   arg-value)
                'set-kernel-arg))

#| Flush and Finish APIs |#

@export
(defun flush (command-queue)
  (check-result (cl-flush command-queue)
                'flush))

@export
(defun finish (command-queue)
  (check-result (cl-finish command-queue)
                'finish))

#| Enqueued Commands APIs |#

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
  (check-result (cl-enqueue-ndrange-kernel command-queue
                                           kernel
                                           work-dim
                                           global-work-offset
                                           global-work-size
                                           local-work-size
                                           num-events-in-wait-list
                                           event-wait-list
                                           event)
                'enqueue-ndrange-kernel))

@export
(defun enqueue-read-buffer (command-queue
                            buffer
                            blocking-read
                            offset
                            size
                            ptr
                            &optional
                              (num-events-in-wait-list 0)
                              (event-wait-list (null-pointer))
                              (event (null-pointer)))
  (check-result (cl-enqueue-read-buffer command-queue
                                        buffer
                                        blocking-read
                                        offset
                                        size
                                        ptr
                                        num-events-in-wait-list
                                        event-wait-list
                                        event)
                'enqueue-read-buffer))

@export
(defun enqueue-write-buffer (command-queue
                             buffer
                             blocking-write
                             offset
                             size
                             ptr
                             &optional
                               (num-events-in-wait-list 0)
                               (event-wait-list (null-pointer))
                               (event (null-pointer)))
  (check-result (cl-enqueue-write-buffer command-queue
                                         buffer
                                         blocking-write
                                         offset
                                         size
                                         ptr
                                         num-events-in-wait-list
                                         event-wait-list
                                         event)
                'enqueue-write-buffer))

@export
(defun enqueue-copy-buffer (command-queue
                            src-buffer
                            dst-buffer
                            src-offset
                            dst-offset
                            size
                            &optional
                              (num-events-in-wait-list 0)
                              (event-wait-list (null-pointer))
                              (event (null-pointer)))
  (check-result (cl-enqueue-copy-buffer command-queue
                                        src-buffer
                                        dst-buffer
                                        src-offset
                                        dst-offset
                                        size
                                        num-events-in-wait-list
                                        event-wait-list
                                        event)
                'enqueue-copy-buffer))

#| parameter setup |#

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
