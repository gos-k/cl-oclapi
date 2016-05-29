#|
  This file is a part of cl-oclapi project.
  Copyright (c) 2016 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-oclapi-test.copy
  (:use :cl
        :cl-oclapi
        :prove
        :cffi
        :cl-oclapi-test.init))
(in-package :cl-oclapi-test.copy)

(plan nil)

(defparameter *copy-kernel*
  "__kernel void copy(__global char * a, __global char * b) { a[0] = b[0]; }")

(subtest "Copy buffer"
  (with-platform-id (platform)
    (ok platform "get platform")
    (with-device-ids (devices num-devices platform)
      (with-context (context (null-pointer) 1 devices)
        (let ((device (mem-aref devices 'cl-device-id)))
          (ok context "create context")
          (with-buffers ((in context +cl-mem-read-only+ 1)
                         (out context +cl-mem-write-only+ 1))
            (ok in "create buffer")
            (ok out "create buffer")
            (with-command-queue (command-queue context device 0)
              (ok command-queue "create command queue")
              (enqueue-copy-buffer command-queue in out 0 0 1)
              (finish command-queue))))))))

(subtest "Copy kernel"
  (with-platform-id (platform)
    (ok platform "get platform")
    (with-device-ids (devices num-devices platform)
      (with-context (context (null-pointer) 1 devices)
        (let ((device (mem-aref devices 'cl-device-id)))
          (ok context "create context")
          (with-buffers ((buffer-in context +cl-mem-read-only+ 1)
                         (buffer-out context +cl-mem-write-only+ 1))
            (ok buffer-in "create buffer-in")
            (ok buffer-out "create buffer-out")
            (with-program-with-source (program context 1 *copy-kernel*)
              (ok program "create program")
              (build-program program
                             1

                             devices)
              (with-kernel (kernel program "copy")
                (ok kernel "create kernel")
                (with-pointers ((p-in buffer-in)
                                (p-out buffer-out))
                  (set-kernel-arg kernel 0 8 p-out)
                  (set-kernel-arg kernel 1 8 p-in)
                  (with-command-queue (command-queue context device 0)
                    (ok command-queue "create command queue")
                    (with-foreign-object (value 'cl-char)
                      (with-foreign-objects ((offset 'cl-size)
                                             (size 'cl-size))
                        (setf (mem-aref offset 'cl-size) 0
                              (mem-aref size 'cl-size) 1)
                        (setf (mem-aref value 'cl-char) 1)
                        (enqueue-write-buffer command-queue
                                              buffer-in
                                              1
                                              (mem-aref offset 'cl-size)
                                              (mem-aref size 'cl-size)
                                              value)
                        (setf (mem-aref value 'cl-char) 2)
                        (enqueue-write-buffer command-queue
                                              buffer-out
                                              1
                                              (mem-aref offset 'cl-size)
                                              (mem-aref size 'cl-size)
                                              value))
                      (with-work-sizes ((global-work-size 1)
                                        (local-work-size 1))
                        (enqueue-ndrange-kernel command-queue
                                                kernel
                                                1
                                                global-work-size
                                                local-work-size)))
                    (finish command-queue)
                    (with-foreign-object (value 'cl-char)
                      (setf (mem-aref value 'cl-char) 0)
                      (enqueue-read-buffer command-queue buffer-out 1 0 1 value)
                      (is 1 (mem-aref value 'cl-char) "buffer-out result"))))))))))))

(finalize)
