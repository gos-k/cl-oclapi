#|
  This file is a part of cl-oclapi project.
  Copyright (c) 2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-oclapi
  (:use :cl
        :cffi
        :cl-reexport))
(in-package :cl-oclapi)

(reexport-from :cl-oclapi.constants)
(reexport-from :cl-oclapi.types)
(reexport-from :cl-oclapi.functions)
(reexport-from :cl-oclapi.helper)

(define-foreign-library libopencl
  (t (:default "libOpenCL")))

(use-foreign-library libopencl)
