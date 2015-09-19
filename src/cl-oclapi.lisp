(in-package :cl-user)
(defpackage cl-oclapi
  (:use :cl :cffi))
(in-package :cl-oclapi)

(define-foreign-library libopencl
  (t (:default "libOpenCL")))

(use-foreign-library libopencl)
