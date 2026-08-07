;;;; Quicklisp third-party deps for workspace-sibling demos.
;;;; tomlet is OCI-only — load via scripts/load-oci-pins.lisp first.
(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&UNHANDLED: ~A~%" c)
        (uiop:quit 1)))

(ql:quickload
 '("alexandria" "babel" "bordeaux-threads" "blackbird" "usocket" "quri"
   "dexador" "chipz" "cl-base64" "cl-ppcre" "ironclad" "jose"
   "clingon" "log4cl" "com.inuoe.jzon" "yason" "dbd-sqlite3" "cl-dbi"
   "websocket-driver" "clack" "clack-handler-hunchentoot" "hunchentoot"
   "trivial-gray-streams" "cffi" "cl+ssl" "rove" "trivial-garbage"
   "local-time" "closer-mop" "split-sequence" "trivial-utf-8" "assoc-utils"
   "cl-json" "fast-io" "static-vectors" "trivial-features" "alexandria")
 :silent t)

(format t "~&; ql-deps done~%")
(uiop:quit 0)
