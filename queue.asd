(defpackage :queue-asd
  (:use :cl :asdf))

(in-package :queue-asd)

(defsystem :queue
  :name "queue"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "Simple FIFO for Common Lisp."
  :serial t
  :components ((:file "queue")))
