(in-package #:harness-demos-image)

;;; Demo 12: comments and layout of the code edit-insert / edit-replace write
;;; (lisp-source-protocol#11, #14, #15 and lisp-mcp#28; fixed in
;;; lisp-source-protocol 0.2.6, picked up by lisp-mcp 0.7.3). Before the fix,
;;; edit-insert dropped every comment of its code and moved the anchor's
;;; trailing comment onto the new form. COND clauses came out one column in,
;;; a replaced LABELS binding indented like a call, and a form inserted first
;;; or last in a list landed on the anchor's line.

(defparameter +probe-file+
  (format nil "(in-package #:cl-user)~%~%(defun probe-a () 1)~%"))

(defparameter +commented-code+
  ";;; Section comment before the first inserted form.

(defun probe-b ()
  ;; inner comment
  2)

;;; Comment between forms.

(defun probe-c () 3)"
  "lisp-mcp#28's code. Three section headers were lost this way in one day.")

(defparameter +step-file+
  "(in-package #:cl-user)

(defun %ssl-step (ret)
  (labels ((touch ()
             (setf ret 2)))
    (touch)))
")

(defparameter +route-hop-code+
  "(route-hop ()
\"Doc.\"
(cond
((plusp ret) (values :ok ret))
(t (values :error ret))))"
  "A LABELS binding with a COND, written flush left as an agent might send it.")

(defparameter +step-after-insert+
  "(in-package #:cl-user)

(defun %ssl-step (ret)
  (labels ((route-hop ()
             \"Doc.\"
             (cond
               ((plusp ret) (values :ok ret))
               (t (values :error ret))))
           (touch ()
             (setf ret 2)))
    (touch)))
")

(defparameter +step-after-replace+
  "(in-package #:cl-user)

(defun %ssl-step (ret)
  (labels ((route-hop ()
             \"Doc.\"
             (cond
               ((plusp ret) (values :ok ret))
               (t (values :error ret))))
           (touch ()
             \"Count a step.\"
             (incf ret))) ; one more
    (touch)))
")

(defparameter +step-after-last+
  "(in-package #:cl-user)

(defun %ssl-step (ret)
  (labels ((route-hop ()
             \"Doc.\"
             (cond
               ((plusp ret) (values :ok ret))
               (t (values :error ret))))
           (touch ()
             \"Count a step.\"
             (incf ret)) ; one more
           (done () ret))
    (touch)))
")

(defparameter +proxy-file+
  "(in-package #:cl-user)

(defun connect (proxy)
  (let* ((host (proxy-host proxy))
         (http-connect-p nil) ; HTTPS via HTTP proxy → CONNECT then TLS
         (port 443))
    (list host http-connect-p port)))
")

(defparameter +proxy-code+
  "(proxy-tls-p nil) ; TLS to the proxy itself
(proxy-auth nil)  ; Proxy-Authorization header"
  "lisp-source-protocol#14 item 3: both comments were dropped, and the anchor's moved.")

(defparameter +proxy-result+
  "(in-package #:cl-user)

(defun connect (proxy)
  (let* ((host (proxy-host proxy))
         (http-connect-p nil) ; HTTPS via HTTP proxy → CONNECT then TLS
         (proxy-tls-p nil) ; TLS to the proxy itself
         (proxy-auth nil)  ; Proxy-Authorization header
         (port 443))
    (list host http-connect-p port)))
")

(defun demo-edit-layout ()
  (banner "Demo 12: comments and layout of edit code (lisp-source-protocol#11, #14, #15; lisp-mcp#28)")
  (let* ((root (make-project "layout" `(("probe.lisp" ,+probe-file+)
                                        ("step.lisp" ,+step-file+)
                                        ("proxy.lisp" ,+proxy-file+))))
         (tools (lt:make-lisp-tools :root root))
         (ok t))
    (labels ((check (label value)
               (show label (and value t))
               (setf ok (and value ok)))
             (edit (label file expected tool-name &rest args)
               (show (format nil "~a before" file) (read-file root file))
               (show "code" (getf args :code ""))
               (let ((kv (loop for (k v) on args by #'cddr
                               collect (string-downcase k) collect v)))
                 (let ((dry (apply #'tool tools tool-name "path" file "dry_run" t "verbose" t kv)))
                   (when (stringp dry)
                     (show "the whole dry-run diff" (subseq dry (or (search "---" dry) 0)))))
                 (apply #'tool tools tool-name "path" file kv))
               (let ((text (read-file root file)))
                 (show (format nil "~a after" file) text)
                 (check label (equal text expected))
                 (check (format nil "~a reads clean" file)
                        (%has "clean" (tool tools "source-check" "path" file "rules" "none")))))
             (edit-arg (label file expected tool-name locator code &rest more)
               (apply #'edit label file expected tool-name :locator locator :code code more)))
      (unwind-protect
           (progn
             (stage "edit-insert keeps the comments of its code (lisp-mcp#28, lisp-source-protocol#11)")
             (edit-arg "both section comments and the inner one reach the file, as written"
                       "probe.lisp" (format nil "~a~%~a~%" (string-right-trim '(#\Newline) +probe-file+)
                                            (format nil "~%~a" +commented-code+))
                       "edit-insert" "defun probe-a" +commented-code+)
             (stage "a binding inserted first gets its own line; COND clauses go 2 in (#15, #14 items 1-2)")
             (edit-arg "route-hop on its own line, touch below it, COND clauses 2 in"
                       "step.lisp" +step-after-insert+ "edit-insert" "path:1.3.1.0" +route-hop-code+
                       :where "before")
             (stage "a LABELS binding replaced whole indents like a local function (#14 item 2)")
             (edit-arg "the body 2 in from the binding, the trailing comment after the closers"
                       "step.lisp" +step-after-replace+ "edit-replace" "path:1.3.1.1"
                       (format nil "(touch ()~%\"Count a step.\"~%(incf ret)) ; one more"))
             (stage "a binding inserted last gets its own line; the comment stays on touch's line (#15)")
             (edit-arg "done on its own line after touch, '; one more' still on touch's line"
                       "step.lisp" +step-after-last+ "edit-insert" "path:1.3.1.1" "(done () ret)")
             (stage "bindings with trailing comments after one with a comment (#14 item 3)")
             (edit-arg "every comment where it was written, the anchor's on the anchor"
                       "proxy.lisp" +proxy-result+ "edit-insert" "path:1.3.1.1" +proxy-code+)
             ok)
        (lt:close-lisp-tools tools)))))
