(asdf:load-system "cl-repository-client")
(format t "dir=~A~%" (asdf:system-source-directory "cl-repository-client"))
(format t "ensure=~A~%"
        (fboundp (find-symbol "ENSURE-SYSTEM-DEPENDENCIES" :cl-repo)))
(uiop:quit 0)
