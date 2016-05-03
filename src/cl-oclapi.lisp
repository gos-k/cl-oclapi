(in-package :cl-user)
(defpackage cl-oclapi
  (:use :cl
        :cffi
        :cl-reexport))
(in-package :cl-oclapi)

(reexport-from :cl-oclapi.constants)
(reexport-from :cl-oclapi.types)
(reexport-from :cl-oclapi.functions)

(define-foreign-library libopencl
  (t (:default "libOpenCL")))

(use-foreign-library libopencl)
