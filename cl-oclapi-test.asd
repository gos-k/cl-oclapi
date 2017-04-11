#|
  This file is a part of cl-oclapi project.
  Copyright (c) 2015-2017 gos-k (mag4.elan@gmail.com)
|#

(defsystem "cl-oclapi-test"
  :author "gos-k"
  :license "MIT"
  :depends-on ("cl-oclapi"
               "prove"
               "cl-annot")
  :components ((:module "t"
                :components
                ((:file "init")
                 (:test-file "bindings")
                 (:test-file "copy"))))
  :description "Test system for cl-oclapi"

  :defsystem-depends-on ("prove-asdf")
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
