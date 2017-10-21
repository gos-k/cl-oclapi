#|
  This file is a part of cl-oclapi project.
  Copyright (c) 2015-2017 gos-k (mag4.elan@gmail.com)
|#

#|
  Author: gos-k (mag4.elan@gmail.com)
|#

(defsystem "cl-oclapi"
  :version "0.1"
  :author "gos-k"
  :license "MIT"
  :depends-on ("alexandria"
               "cffi"
               "cl-annot"
               "cl-reexport")
  :components ((:module "src"
                        :components
                        ((:module "bindings"
                                  :components
                                  ((:file "constants")
                                   (:file "types")
                                   (:file "functions")
                                   (:file "information")))
                         (:module "helpers"
                                  :components
                                  ((:file "safe-call")
                                   (:file "with-macro")
                                   (:file "memory")))
                         (:file "cl-oclapi"))))
  :description "binding for OpenCL API"
  ;; :long-description #.(read-file-string (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "cl-oclapi-test"))))
