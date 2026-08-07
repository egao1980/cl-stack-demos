;;;; Inspired by CPython asyncio docs — sleep* + callback on event loop.
(in-package #:cl-stack-demos)

(defun run-asyncio-sleep ()
  (asdf:load-system "event-backend-libuv")
  (let* ((backend (event-backend-libuv:make-libuv-backend))
         (loop (event-protocol:make-event-loop backend))
         (done nil)
         (order nil))
    (event-protocol:with-event-backend (backend)
      (event-protocol:with-event-loop-var (loop)
        (event-protocol:defer backend loop
          (lambda ()
            (push :start order)
            (event-protocol:sleep* backend loop 0.05
                                   :callback
                                   (lambda ()
                                     (push :awake order)
                                     (setf done t)
                                     (event-protocol:stop backend loop)))))
        (event-protocol:run backend loop :stop-when-idle t)))
    (format t "~&; order=~S~%" (reverse order))
    (assert done)
    (assert (equal (reverse order) '(:start :awake)))
    t))

(register-app "asyncio-sleep"
              :title "asyncio.sleep-shaped event-loop defer+sleep*"
              :upstream "https://github.com/python/cpython (asyncio)"
              :fn #'run-asyncio-sleep)
