;;;; Parallelism primitives, built on BORDEAUX-THREADS.

(defpackage #:parfait
  (:use :cl :bordeaux-threads)
  (:export #:deref
           #:promise
           #:deliver
           #:future
           #:delay))

(defpackage #:parfait.semaphore
  (:use :cl :bordeaux-threads)
  (:export #:sem-make
           #:sem-count
           #:sem-wait
           #:sem-try-wait
           #:sem-signal
           #:with-sem-wait))
