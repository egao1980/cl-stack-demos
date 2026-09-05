;;;; Inspired by python-websockets/websockets example/sync/{echo,client}.py
;;;; Server side uses ws-protocol make-ws-server / start-ws-server.
(in-package #:cl-stack-demos)

(defun run-websockets-echo ()
  (asdf:load-system "ws-backend-websocket-driver")
  (let ((server nil)
        (port nil)
        (payload (format nil "ws-demo-~A" (get-universal-time)))
        (got nil)
        (err nil))
    (labels ((start ()
               (loop for attempt from 1 to 8
                     for p = (+ 19000 (random 3000))
                     do (handler-case
                            (let ((backend (ws-backend-websocket-driver:make-websocket-driver-backend)))
                              (setf server
                                    (ws-protocol:make-ws-server
                                     backend
                                     :host "127.0.0.1"
                                     :port p
                                     :path "/echo"
                                     :on-connect
                                     (lambda (conn)
                                       (ws:on conn :message
                                              (lambda (msg)
                                                (ws:send conn msg)))))
                                    port p)
                              (ws-protocol:start-ws-server server :background t)
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
        (when server
          (ignore-errors (ws-protocol:stop-ws-server server))
          (sleep 0.1))))))

(register-app "websockets-echo"
              :title "websockets sync echo client/server"
              :upstream "https://github.com/python-websockets/websockets"
              :fn #'run-websockets-echo)
