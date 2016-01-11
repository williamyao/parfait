(defpackage #:parfait/test
  (:use :cl :fiveam :parfait :parfait.semaphore :bordeaux-threads)
  (:export parfait/test-suite))
(in-package #:parfait/test)

(def-suite parfait/test-suite :description "All tests for Parfait.")

