;;;; OCI pins that are not on Quicklisp (or preferred from GHCR).
(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&UNHANDLED: ~A~%" c)
        (uiop:quit 1)))

(asdf:load-system "cl-repository-client")
(cl-repo:add-registry "https://ghcr.io" :namespace "egao1980/cl-systems" :priority :prepend)

;; Source-only pins used by demos (siblings override when present on ASDF path).
(dolist (pair '(("tomlet" . "0.1.0")
                ("jose" . "0.1.0")))
  (format t "~&; OCI ~A:~A~%" (car pair) (cdr pair))
  (finish-output)
  (cl-repo:load-system (car pair) :version (cdr pair)))

(format t "~&; load-oci-pins done~%")
(uiop:quit 0)
