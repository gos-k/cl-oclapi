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

(defmacro defun-check-result (name (&rest args) &body body)
  `(defun ,name (,@args)
     (check-result (progn
                     ,@body)
                   ',name)))

#| Platform APIs |#

@export
(defun-check-result get-platform-ids (num platforms num-platforms)
  (cl-get-platform-ids num platforms num-platforms))

#| Device APIs |#

@export
(defun-check-result get-device-ids (platform device-type num-entries devices num-devices)
  (cl-get-device-ids platform device-type num-entries devices num-devices))

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

@export
(defun-check-result release-context (context)
  (cl-release-context context))

#| Command Queue APIs |#

@export
(defun create-command-queue (context device properties)
  (with-foreign-object (errcode-ret 'cl-int)
    (let ((command-queue (cl-create-command-queue context
                                                  device
                                                  properties
                                                  errcode-ret)))
      (check-errcode-ret command-queue 'cl-create-command-queue errcode-ret))))

@export
(defun-check-result release-command-queue (command-queue)
  (cl-release-command-queue command-queue))

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

@export
(defun-check-result release-mem-object (memobj)
  (cl-release-mem-object memobj))

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
(defun-check-result release-program (program)
  (cl-release-program program))

@export
(defun-check-result build-program (program
                                   num-devices
                                   device-list
                                   &optional
                                   (options (null-pointer))
                                   (cl-callback (null-pointer))
                                   (user-data (null-pointer)))
  (cl-build-program program
                    num-devices
                    device-list
                    options
                    cl-callback
                    user-data))

#| Kernel Object APIs |#

@export
(defun create-kernel (program kernel-name)
  (with-foreign-object (errcode-ret 'cl-int)
    (let ((kernel (cl-create-kernel program kernel-name errcode-ret)))
      (check-errcode-ret kernel 'cl-create-kernel errcode-ret))))

@export
(defun-check-result set-kernel-arg (kernel arg-index arg-size arg-value)
  (cl-set-kernel-arg kernel
                     arg-index
                     arg-size
                     arg-value))

@export
(defun-check-result release-kernel (kernel)
  (cl-release-kernel kernel))

#| Flush and Finish APIs |#

@export
(defun-check-result flush (command-queue)
  (cl-flush command-queue))

@export
(defun-check-result finish (command-queue)
  (cl-finish command-queue))

#| Enqueued Commands APIs |#

@export
(defun-check-result enqueue-ndrange-kernel (command-queue
                                            kernel
                                            work-dim
                                            global-work-offset
                                            global-work-size
                                            local-work-size
                                            &optional
                                            (num-events-in-wait-list 0)
                                            (event-wait-list (null-pointer))
                                            (event (null-pointer)))
  (cl-enqueue-ndrange-kernel command-queue
                             kernel
                             work-dim
                             global-work-offset
                             global-work-size
                             local-work-size
                             num-events-in-wait-list
                             event-wait-list
                             event))

@export
(defun-check-result enqueue-read-buffer (command-queue
                                         buffer
                                         blocking-read
                                         offset
                                         size
                                         ptr
                                         &optional
                                         (num-events-in-wait-list 0)
                                         (event-wait-list (null-pointer))
                                         (event (null-pointer)))
  (cl-enqueue-read-buffer command-queue
                          buffer
                          blocking-read
                          offset
                          size
                          ptr
                          num-events-in-wait-list
                          event-wait-list
                          event))

@export
(defun-check-result enqueue-write-buffer (command-queue
                                          buffer
                                          blocking-write
                                          offset
                                          size
                                          ptr
                                          &optional
                                          (num-events-in-wait-list 0)
                                          (event-wait-list (null-pointer))
                                          (event (null-pointer)))
  (cl-enqueue-write-buffer command-queue
                           buffer
                           blocking-write
                           offset
                           size
                           ptr
                           num-events-in-wait-list
                           event-wait-list
                           event))

@export
(defun-check-result enqueue-copy-buffer (command-queue
                                         src-buffer
                                         dst-buffer
                                         src-offset
                                         dst-offset
                                         size
                                         &optional
                                         (num-events-in-wait-list 0)
                                         (event-wait-list (null-pointer))
                                         (event (null-pointer)))
  (cl-enqueue-copy-buffer command-queue
                          src-buffer
                          dst-buffer
                          src-offset
                          dst-offset
                          size
                          num-events-in-wait-list
                          event-wait-list
                          event))

#| parameter setup |#

@export
(defun set-platform-id (properties platform-id)
  (setf (mem-aref properties 'cl-context-properties 0) +cl-context-platform+)
  (setf (mem-aref properties 'cl-platform-id 1) platform-id)
  (setf (mem-aref properties 'cl-context-properties 2) 0))

@export
(defun set-work-size (work-size x &optional (y 0) (z 0))
  (setf (mem-aref work-size 'cl-size 0) x
        (mem-aref work-size 'cl-size 1) y
        (mem-aref work-size 'cl-size 2) z))
