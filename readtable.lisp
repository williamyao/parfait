;;;; Addition of mergeable readtable for easier usage of DEREF.

(in-package #:parfait)

(export 'deref-readtable)

(named-readtables:defreadtable deref-readtable
  (:macro-char #\^ (lambda (stream char)
                     (declare (ignore char))
                     (list 'deref (read stream t nil t)))))
