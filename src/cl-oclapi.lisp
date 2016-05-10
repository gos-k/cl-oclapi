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

(eval-when (:load-toplevel)
  (define-foreign-library libopencl
    (t (:default "libOpenCL")))

  (unless (foreign-library-loaded-p 'libopencl)
    (use-foreign-library libopencl)))
