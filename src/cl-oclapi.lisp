(in-package :cl-user)
(defpackage cl-oclapi
  (:use :cl :cffi))
(in-package :cl-oclapi)

(define-foreign-library libopencl
  (t (:default "libOpenCL")))

(use-foreign-library libopencl)

#| cl-platform.h |#
(defctype cl-char :int8)
(defctype cl-uchar :uint8)
(defctype cl-short :int16)
(defctype cl-ushort :uint16)
(defctype cl-int :int32)
(defctype cl-uint :uint32)
(defctype cl-long :int64)
(defctype cl-ulong :uint64)

(defctype cl-half :uint16)
(defctype cl-float :float)
(defctype cl-double :double)

(defctype size-t :uint64)
(defctype intptr-t :int64)
(defctype uintptr-t :uint64)
