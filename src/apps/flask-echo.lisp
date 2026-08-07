;;;; Inspired by pallets/flask tutorial shape — health + JSON echo.
(in-package #:cl-stack-demos)

(defun run-flask-echo ()
  (asdf:load-system "http-server-backend-hunchentoot")
  (asdf:load-system "json-backend-jzon")
  (labels ((slurp (stream)
             (when stream
               (with-output-to-string (out)
                 (loop for c = (read-char stream nil nil)
                       while c do (write-char c out)))))
           (app (env)
             (let ((method (getf env :request-method))
                   (path (getf env :path-info)))
               (cond
                 ((and (eq method :get) (string= path "/health"))
                  '(200 (:content-type "text/plain") ("ok")))
                 ((and (eq method :post) (string= path "/echo"))
                  (let* ((raw (or (slurp (getf env :raw-body)) "{}"))
                         (data (stack-json:decode raw)))
                    (list 200 '(:content-type "application/json")
                          (list (stack-json:encode data)))))
                 (t '(404 (:content-type "text/plain") ("nope")))))))
    (let ((port (%ephemeral-port)))
      (stack-http-server:with-server (s #'app :host "127.0.0.1" :port port)
        (sleep 0.1)
        (let ((base (format nil "http://127.0.0.1:~A"
                            (stack-http-server:server-port s))))
          (stack-http:with-backend (:dexador)
            (let* ((h (stack-http:get (format nil "~A/health" base)))
                   (e (stack-http:post (format nil "~A/echo" base)
                                       :json '(("q" . 1))))
                   (ej (stack-http:response-json e)))
              (format t "~&; health=~A echo.q=~A~%"
                      (stack-http:response-text h)
                      (gethash "q" ej))
              (assert (string= (string-trim '(#\Space #\Newline)
                                            (stack-http:response-text h))
                               "ok"))
              (assert (= 1 (gethash "q" ej)))
              t)))))))

(register-app "flask-echo"
              :title "Flask-shaped health + JSON echo"
              :upstream "https://github.com/pallets/flask"
              :fn #'run-flask-echo)
