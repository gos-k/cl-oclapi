#|
  This file is a part of cl-oclapi project.
  Copyright (c) 2015 gos-k (mag4.elan@gmail.com)
|#

#|
  Author: gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-oclapi-asd
  (:use :cl :asdf))
(in-package :cl-oclapi-asd)

(defsystem cl-oclapi
  :version "0.1"
  :author "gos-k"
  :license "MIT"
  :depends-on (:alexandria
               :cffi
               :cl-annot
               :cl-reexport)
  :components ((:module "src"
                        :components
                        ((:module "bindings"
                                  :components
                                  ((:file "constants")
                                   (:file "types")
                                   (:file "functions")))
                         (:module "helpers"
                                  :components
                                  ((:file "safe-call")
                                   (:file "with-macro")
                                   (:file "memory")))
                         (:file "cl-oclapi"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-oclapi-test))))
