(in-package :cl-user)
(defpackage cl-oclapi-test.init
  (:use :cl
        :cl-annot
        :prove
        :cffi
        :cl-oclapi))
(in-package :cl-oclapi-test.init)

(annot:enable-annot-syntax)

@export
(defmacro is-success (got &optional desc)
  `(is ,got +cl-success+ ,desc))

@export
(defmacro is-null (got &optional desc)
  `(ok (null-pointer-p ,got) ,desc))
