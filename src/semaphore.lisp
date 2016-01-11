;;;; Implementation of semaphores, with the ability to increment
;;;; and decrement by arbitrary amounts.

(in-package #:parfait.semaphore)

(defclass semaphore ()
  ((count :reader sem-count :initform 0 :initarg :count)
   (lock :reader lock :initform (make-recursive-lock))
   (blocked :initform (%qmake))))

(defun sem-make (&optional (count 0))
  "Return a new semaphore, with the given count. If COUNT is not
   given, defaults to 0."
  (assert (not (minusp count)) (count)
          "Cannot create a semaphore with negative initial count (~D)." count)
  (make-instance 'semaphore
                 :count count))

(defun sem-wait (sem &optional (count 1))
  "Attempts to decrement the semaphore's count. If the amount to decrement
   is greater than the semaphore's count, decrement as much as possible and
   block until the rest of the amount can be fulfilled."
  (assert (not (minusp count)) (count)
          "Cannot SEM-WAIT on a negative count (~D)." count)
  (with-lock-held ((lock sem))
    (with-slots ((sem-count count) blocked) sem
      (if (>= sem-count count)
          (decf sem-count count)
          (let ((condvar (make-condition-variable)))
            (%qenq blocked (cons (- count sem-count)
                                 condvar))
            (setf sem-count 0)
            (condition-wait condvar (lock sem)))))))

(defun sem-try-wait (sem &optional (count 1))
  "Attempt to wait on the semaphore; return T if successfully
   decremented the semaphore's count, and NIL otherwise."
  (assert (not (minusp count)) (count)
          "Cannot SEM-TRY-WAIT on a negative count (~D)." count)
  (with-lock-held ((lock sem))
    (with-slots ((sem-count count)) sem
      (when (>= sem-count count)
        (decf sem-count count)
        t))))

(defun sem-signal (sem &optional (count 1))
  "Increment the semaphore's count, unblocking any blocked threads
   in the order they were blocked."
  (assert (not (minusp count)) (count)
          "Semaphore cannot be incremented by a negative number (~D)." count)
  (with-recursive-lock-held ((lock sem))
    (with-slots ((sem-count count) blocked) sem
      (cond ((<= count 0))
            ((%qempty? blocked) (incf sem-count count))
            (t (destructuring-bind (req . condvar) (%qfront blocked)
                 (if (< count req)
                     (rplaca (%qfront blocked) (- req count))
                     (progn (%qdeq blocked)
                            (condition-notify condvar)
                            (sem-signal sem (- count req))))))))))

(defmacro with-sem-wait ((sem &optional (count 1)) &body body)
  "Attempt to decrement the semaphore, blocking when unsuccessful, and
   run and return the value of BODY, finally signalling the semaphore by
   the same count."
  (alexandria:once-only (sem count)
    `(progn (sem-wait ,sem ,count)
            (prog1 (progn ,@body)
              (sem-signal ,sem ,count)))))

(defmethod print-object ((obj semaphore) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream ":SEM-COUNT ~D" (sem-count obj))))

;;; Queue operations.
(defun %qmake () (cons nil nil))
(defun %qempty? (q) (and (null (car q))
                         (null (cdr q))))
(defun %qfront (q) (caar q))
(defun %qenq (q obj)
  (let ((tail (cdr q))
        (cell (cons obj nil)))
    (if tail
        (progn (rplacd tail cell)
               (rplacd q (cdr tail)))
        (progn (rplaca q cell)
               (rplacd q cell)))))
(defun %qdeq (q)
  (symbol-macrolet ((head (car q))
                    (tail (cdr q)))
    (prog1 (car head)
      (if (eq head tail)
          (setf head nil tail nil)
          (setf head (cdr head))))))
