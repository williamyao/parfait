;;;; Primitives for working with parallelism.
;;;;
;;;; Inspired by Clojure's core parallelism primitives.

(defpackage #:parfait/asdf
  (:use :cl :asdf))
(in-package #:parfait/asdf)

(defsystem #:parfait
  (:name "Parfait")
  (:description "Primitives for working with parallelism.")
  (:author "William Yao <williamyaoh@gmail.com>")
  (:maintainer "William Yao <williamyaoh@gmail.com>")
  (:depends-on (:alexandria :bordeaux-threads :named-readtables))
  (:serial t)
  (:components (:file "parfait")))
