(in-package #:parfait/test)
(in-suite parfait/test-suite)

(def-test queue ()
  (let ((q (parfait.semaphore::%qmake)))
    ;; Test that an empty queue return NIL on dequeue.
    (is (eql (parfait.semaphore::%qdeq q) nil))
    (is (parfait.semaphore::%qempty? q))
    (is (eql (parfait.semaphore::%qfront q) nil)))

  (let ((q (parfait.semaphore::%qmake)))
    ;; Test that a single enqueue and dequeue works properly.
    (finishes (parfait.semaphore::%qenq q :first))
    
    (is (not (parfait.semaphore::%qempty? q)))
    (is (eql (parfait.semaphore::%qfront q) :first))
    (is (eql (parfait.semaphore::%qdeq q) :first)))

  (let ((q (parfait.semaphore::%qmake)))
    ;; Test multiple enqueues and dequeues.
    (finishes (parfait.semaphore::%qenq q :first))
    (finishes (parfait.semaphore::%qenq q :second))
    
    (is (not (parfait.semaphore::%qempty? q)))
    (is (eql (parfait.semaphore::%qfront q) :first))
    (is (eql (parfait.semaphore::%qdeq q) :first))

    (finishes (parfait.semaphore::%qenq q :third))

    (is (eql (parfait.semaphore::%qfront q) :second))
    (is (eql (parfait.semaphore::%qdeq q) :second))
    (is (eql (parfait.semaphore::%qfront q) :third))
    (is (eql (parfait.semaphore::%qdeq q) :third))))

(def-test semaphore ()
  (let ((s (sem-make 0))
        (val 0))
    ;; Test that blocking works.
    (make-thread (lambda () (sem-wait s) (incf val 2)))
    (sleep 1)
    (make-thread (lambda () (sem-wait s) (incf val 1)))
    
    (is (= val 0))

    ;; Test that signalling works, and that threads wake up in
    ;; the order that they blocked.
    (finishes (sem-signal s))
    (sleep 1)
    (is (= val 2))

    (finishes (sem-signal s))
    (sleep 1)
    (is (= val 3)))

  (let ((s (sem-make 1))
        (val 0))
    ;; Test that waiting with higher counts works, and that they still
    ;; get queued in order.
    (make-thread (lambda () (sem-wait s 5) (incf val 2)))
    (sleep 1)
    (make-thread (lambda () (sem-wait s 1) (incf val 1)))

    (is (= val 0))

    (finishes (sem-signal s 1))
    (sleep 1)
    (is (= val 0))

    (finishes (sem-signal s 3))
    (sleep 1)
    (is (= val 2))

    (finishes (sem-signal s 1))
    (sleep 1)
    (is (= val 3)))

  (let ((s (sem-make 1)))
    ;; Test SEM-TRY-WAIT.
    (is (not (sem-try-wait s 2)))
    (is (= (sem-count s) 1))

    (is (sem-try-wait s 1))
    (is (= (sem-count s) 0))))
