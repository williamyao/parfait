;;;; Parallelism primitives, built on BORDEAUX-THREADS.

(defpackage #:parfait
  (:use :cl :bordeaux-threads)
  (:export #:deref
           #:promise
           #:deliver
           #:future
           #:delay))
