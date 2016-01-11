;;;; Unit tests. Duh.

;;; This section is a quick shim while the pull request for this is open.
(in-package :it.bese.fiveam)

(defmacro deadline ((ms) &body body)
  "Generates a pass if the body finishes within the given number of
   milliseconds."
  (with-gensyms (duration)
    `(let ((,duration (internal->ms (timer ,@body))))
       (if (<= ,duration ,ms)
           (add-result 'test-passed :test-expr ',body)
           (process-failure
            :reason (format nil "Test supposed to finish within ~Dms, but took ~Dms" ,ms ,duration)
            :test-expr ',body)))))

(defmacro timer (&body body)
  "Return the number of internal real time units that BODY takes to run to
   completion."
  (with-gensyms (beg)
    `(let ((,beg (get-internal-real-time)))
       ,@body
       (- (get-internal-real-time) ,beg))))

(defun internal->ms (internal)
  "Convert internal real time units to milliseconds."
  (floor (/ internal #.(/ internal-time-units-per-second 1000))))

(export 'deadline)

;;; Actual unit test definitions.
(defpackage #:parfait/test
  (:use :cl :fiveam :parfait)
  (:export parfait/test-suite))
(in-package #:parfait/test)

(def-suite parfait/test-suite :description "All tests for Parfait.")
(in-suite parfait/test-suite)

(def-test promise ()
  (let ((p (promise)))
    ;; Test that DELIVERing has the desired effect.
    (deliver p :delivered)
    (deadline (100) (deref p))
    (is (eq (deref p) :delivered))

    ;; Test that multiple DELIVERs have no effect beyond
    ;; the first.
    (handler-bind ((warning (lambda (x)
                              (declare (ignore x))
                              (muffle-warning))))
      (deliver p :delivered-again))
    (deadline (100) (deref p))
    (is (eq (deref p) :delivered))))

(def-test future ()
  (let (p)
    ;; Test that FUTURE returns immediately, even when the spawned
    ;; thread takes large amounts of time.
    (deadline (100) (setf p (future (sleep 3) :delivered)))

    ;; Test that we actually get back the return value of the
    ;; thread when dereferencing. Larger SLEEP value, just in
    ;; case.
    (sleep 5)
    (deadline (100) (deref p))
    (is (eq (deref p) :delivered))

    ;; Test that subsequent DEREFs do not rerun the body.
    (deadline (100) (deref p))
    (is (eq (deref p) :delivered))))

(def-test delay ()
  (let* ((val 0)
         (d (delay (setf val (incf val)))))
    ;; Test that the body has been DELAYed i.e. not executed yet.
    (is (= val 0))

    ;; Test that we actually get back the return value of the DELAY.
    (is (= (deref d) 1))
    (is (= val 1))

    ;; Test that subsequent DEREFs do not rerun the body.
    (is (= (deref d) 1))
    (is (= val 1))))
