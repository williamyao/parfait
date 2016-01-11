;;;; Primitives for working with functions as if they were asynchronous.
;;;;
;;;; Inspired by Clojure's core parallelism primitives.

(in-package #:parfait)

(defclass async-thunk ()
  ((value :accessor value)
   (access :reader access :initform (make-lock))))

(defclass promise (async-thunk)
  ((delivered? :reader delivered? :initform (make-condition-variable))))

(defclass delay (async-thunk)
  ((calc-fn :reader calc-fn :initarg :calc-fn)))

(defgeneric deref (obj)
  (:documentation "Attempt to atomically return the value contained in OBJ.
Blocks if a value is not yet ready."))
(defmethod deref ((obj promise))
  (with-lock-held ((access obj))
    (when (not (slot-boundp obj 'value))
      (condition-wait (delivered? obj) (access obj)))
    (value obj)))
(defmethod deref ((obj delay))
  (with-lock-held ((access obj))
    (when (not (slot-boundp obj 'value))
      (setf (value obj) (funcall (calc-fn obj))))
    (value obj)))

(defun promise () (make-instance 'promise))

(defun deliver (promise value)
  "Atomically place VALUE into PROMISE. DELIVER's to a `promise' after the
   first have no effect."
  (with-lock-held ((access promise))
    (if (slot-boundp promise 'value)
        (warn "Attempting to deliver more than once to ~A." promise)
        (progn (setf (value promise) value)
               (condition-notify (delivered? promise))))))

(defmacro future (&body body)
  "Return a `promise', whose value will be delivered by a new thread."
  (alexandria:with-gensyms (promise-sym)
    `(let ((,promise-sym (promise)))
       (prog1 ,promise-sym
         (make-thread
          (lambda () (deliver ,promise-sym (progn ,@body))))))))

(defmacro delay (&body body)
  `(make-instance 'delay :calc-fn (lambda () ,@body)))

(defmethod print-object ((obj async-thunk) stream)
  (with-lock-held ((access obj))
    (print-unreadable-object (obj stream :type t)
      (if (slot-boundp obj 'value)
          (format stream ":VALUE ~S" (value obj))
          (format stream ":NOT-READY")))))
