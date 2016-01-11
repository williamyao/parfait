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
