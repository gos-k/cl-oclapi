(in-package :cl-user)
(defpackage cl-oclapi-test.init
  (:use :cl
        :cl-annot
        :prove
        :cl-oclapi))
(in-package :cl-oclapi-test.init)

(annot:enable-annot-syntax)

@export
(defmacro is-success (got &optional desc)
  `(is ,got +cl-success+ ,desc))
