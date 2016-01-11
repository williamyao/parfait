;;;; Primitives for working with parallelism.
;;;;
;;;; Inspired by Clojure's core parallelism primitives.

(in-package :asdf/user)

(defsystem "parfait"
  :name "Parfait"
  :description "Primitives for working with parallelism."
  :author "William Yao <williamyaoh@gmail.com>"
  :maintainer "William Yao <williamyaoh@gmail.com>"
  :depends-on ("alexandria" "bordeaux-threads")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "parfait")
               (:file "semaphore"))
  :in-order-to ((test-op (test-op "parfait/test"))))

(defsystem "parfait+readtable"
  :depends-on ("parfait" "named-readtables")
  :pathname "src"
  :components ((:file "readtable")))

(defsystem "parfait/test"
  :depends-on ("parfait" "fiveam")
  :serial t
  :pathname "test"
  :components ((:file "package")
               (:file "tests")
               (:file "sem-tests"))
  :perform (test-op (op sys)
             (uiop:symbol-call :fiveam '#:run!
                               (uiop:find-symbol* :parfait/test-suite
                                                  :parfait/test))))
