(in-package :cl-user)
(defpackage cl-oclapi-test.copy
  (:use :cl
        :cl-oclapi
        :prove
        :cffi))
(in-package :cl-oclapi-test.copy)

(plan nil)

;(defconstant copy-kernel "__kernel void alfa(__global char * a) { a[0] = 1; }")
(defparameter copy-kernel
  "__kernel void alfa(__global char * a) { a[0] = 1; }")
  ;"__kernel void alfa(__global char * a) { }")

(subtest "Copy buffer"
  (with-foreign-objects ((platforms 'cl-platform-id)
                         (num-platforms 'cl-uint)
                         (devices 'cl-device-id)
                         (num-devices 'cl-uint)
                         (errcode-ret 'cl-int))
    (is +cl-success+ (cl-get-platform-ids 1 platforms num-platforms) "create platform")
    (let ((platform (mem-aref platforms 'cl-platform-id)))
      (is +cl-success+ (cl-get-device-ids platform
                                          +cl-device-type-default+
                                          1
                                          devices
                                          num-devices) "create device")
      (let ((context (cl-create-context (null-pointer)
                                        1
                                        devices
                                        (null-pointer)
                                        (null-pointer)
                                        errcode-ret))
            (device (mem-aref devices 'cl-device-id)))
        (is +cl-success+ (mem-aref errcode-ret 'cl-int) "create context")
        (let ((in (cl-create-buffer context
                                    +cl-mem-read-only+
                                    1
                                    (null-pointer)
                                    errcode-ret)))
          (is +cl-success+ (mem-aref errcode-ret 'cl-int) "create buffer")
          (ok in "create buffer")
          (let ((out (cl-create-buffer context
                                     +cl-mem-write-only+
                                     1
                                     (null-pointer)
                                     errcode-ret)))
            (is +cl-success+ (mem-aref errcode-ret 'cl-int) "create buffer")
            (ok out "create buffer")
            (let ((command-queue (cl-create-command-queue context
                                                          device
                                                          0
                                                          errcode-ret)))
              (is +cl-success+ (mem-aref errcode-ret 'cl-int) "create command queue")
              (ok command-queue "create command queue")
              (with-foreign-objects ((src-offset 'size-t)
                                     (dst-offset 'size-t)
                                     (size 'size-t))
                (setf (mem-ref src-offset 'size-t) 0
                      (mem-ref dst-offset 'size-t) 0
                      (mem-ref size 'size-t) 1)
                (is +cl-success+ (cl-enqueue-copy-buffer command-queue
                                                         in
                                                         out
                                                         (mem-ref src-offset 'size-t)
                                                         (mem-ref dst-offset 'size-t)
                                                         (mem-ref size 'size-t)
                                                         0
                                                         (null-pointer)
                                                         (null-pointer)) "enqueue copy buffer"))
              (is +cl-success+ (cl-finish command-queue)))
            (is +cl-success+ (cl-release-mem-object out)))
          (is +cl-success+ (cl-release-mem-object in)))
        (is +cl-success+ (cl-release-context context) "release context")
        (is +cl-success+ (cl-release-device device) "release device")))))

(finalize)
