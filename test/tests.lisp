;;;; Unit tests. Duh.

(in-package #:parfait/test)
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
         (d (delay (incf val))))
    ;; Test that the body has been DELAYed i.e. not executed yet.
    (is (= val 0))

    ;; Test that we actually get back the return value of the DELAY.
    (is (= (deref d) 1))
    (is (= val 1))

    ;; Test that subsequent DEREFs do not rerun the body.
    (is (= (deref d) 1))
    (is (= val 1))))
