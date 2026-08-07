;;;; Inspired by psf/requests Quickstart — JSON GET/POST against local echo (no public net).
(in-package #:cl-stack-demos)

(defun run-requests-json ()
  ;; Local fixture via http-server; exercises stack-http JSON like r.json().
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
                 ((and (eq method :get) (string= path "/get"))
                  (list 200 '(:content-type "application/json")
                        (list (stack-json:encode
                               '(("url" . "/get") ("args" . :null))))))
                 ((and (eq method :post) (string= path "/post"))
                  (let* ((raw (or (slurp (getf env :raw-body)) "{}"))
                         (data (stack-json:decode raw)))
                    (list 200 '(:content-type "application/json")
                          (list (stack-json:encode
                                 `(("json" . ,data) ("url" . "/post")))))))
                 (t '(404 (:content-type "text/plain") ("nope")))))))
    (let ((port (%ephemeral-port)))
      (stack-http-server:with-server (s #'app :host "127.0.0.1" :port port)
        (sleep 0.1)
        (let ((base (format nil "http://127.0.0.1:~A"
                            (stack-http-server:server-port s))))
          (stack-http:with-backend (:dexador)
            (let* ((g (stack-http:get (format nil "~A/get" base)))
                   (gj (stack-http:response-json g))
                   (p (stack-http:post (format nil "~A/post" base)
                                       :json '(("key" . "value"))))
                   (pj (stack-http:response-json p)))
              (format t "~&; GET url=~A~%" (gethash "url" gj))
              (format t "~&; POST json.key=~A~%"
                      (gethash "key" (gethash "json" pj)))
              (assert (string= (gethash "url" gj) "/get"))
              (assert (string= (gethash "key" (gethash "json" pj)) "value"))
              t)))))))

(register-app "requests-json"
              :title "requests Quickstart JSON GET/POST (local)"
              :upstream "https://github.com/psf/requests"
              :fn #'run-requests-json)
