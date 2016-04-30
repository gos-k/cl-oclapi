#|
  This file is a part of cl-oclapi project.
  Copyright (c) 2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-oclapi-test-asd
  (:use :cl :asdf))
(in-package :cl-oclapi-test-asd)

(defsystem cl-oclapi-test
  :author "gos-k"
  :license "MIT"
  :depends-on (:cl-oclapi
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-oclapi")
                 (:test-file "copy"))))
  :description "Test system for cl-oclapi"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
