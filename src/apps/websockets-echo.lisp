;;;; Inspired by python-websockets/websockets example/sync/{echo,client}.py
;;;; Server side uses Clack + websocket-driver (same as ws-protocol scripts/demo.lisp).
(in-package #:cl-stack-demos)

(defun run-websockets-echo ()
  (asdf:load-system "ws-backend-websocket-driver")
  (asdf:load-system "websocket-driver")
  (asdf:load-system "clack")
  (asdf:load-system "clack-handler-hunchentoot")
  (let ((handler nil)
        (port nil)
        (payload (format nil "ws-demo-~A" (get-universal-time)))
        (got nil)
        (err nil))
    (labels ((echo-app (env)
               (let ((path (or (getf env :path-info) "")))
                 (if (search "/echo" path)
                     (let ((wss (websocket-driver.server:make-server env)))
                       (websocket-driver:on :message wss
                                            (lambda (message)
                                              (websocket-driver:send wss message)))
                       (lambda (responder)
                         (declare (ignore responder))
                         (websocket-driver:start-connection wss)))
                     '(404 (:content-type "text/plain") ("nope")))))
             (start ()
               (loop for attempt from 1 to 8
                     for p = (+ 19000 (random 3000))
                     do (handler-case
                            (progn
                              (setf handler
                                    (clack:clackup #'echo-app
                                                   :server :hunchentoot
                                                   :address "127.0.0.1"
                                                   :port p
                                                   :use-thread t
                                                   :debug nil
                                                   :silent t)
                                    port p)
                              (sleep 0.2)
                              (return p))
                          (error (e)
                            (when (= attempt 8) (error e)))))))
      (start)
      (unwind-protect
           (let ((backend (ws-backend-websocket-driver:make-websocket-driver-backend))
                 (url (format nil "ws://127.0.0.1:~A/echo" port)))
             (format t "~&; echo at ~A~%" url)
             (ws:with-connection (conn url :backend backend :transport :http/1.1)
               (ws:on conn :message (lambda (msg) (setf got msg)))
               (ws:on conn :error (lambda (e) (setf err e)))
               (ws:send conn payload)
               (loop repeat 50
                     until (or got err)
                     do (sleep 0.05)))
             (when err (error err))
             (format t "~&; got ~S~%" got)
             (assert (string= got payload))
             t)
        (when handler
          (ignore-errors (clack:stop handler))
          (sleep 0.1))))))

(register-app "websockets-echo"
              :title "websockets sync echo client/server"
              :upstream "https://github.com/python-websockets/websockets"
              :fn #'run-websockets-echo)
