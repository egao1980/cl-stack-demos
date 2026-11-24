(in-package #:harness-demos-image)

;;; Demo 11: #+sbcl / #-sbcl top-level forms through lisp-mcp's source and
;;; edit tools (lisp-source-protocol#9, fixed in lisp-source-protocol 0.2.5,
;;; picked up by lisp-mcp 0.7.2). This image is SBCL, so a #-sbcl form is
;;; excluded under the tools' own features. Before the fix edit-insert dropped
;;; it from inserted code, and outline and locators did not see one in a file.

(defparameter +conditional-file+
  (format nil "(in-package #:cl-user)~%~%(defun %reset-boundary-random-state ()~%  (setf *random-state* (make-random-state t)))~%~%#+sbcl (defun impl () :sbcl)~%~%#-sbcl (defun impl () :other)~%"))

(defparameter +save-hook-branches+
  (format nil "#+sbcl (pushnew '%reset-boundary-random-state sb-ext:*save-hooks*)~%#-sbcl (uiop:register-image-dump-hook '%reset-boundary-random-state)")
  "The two forms http-protocol#58 inserted; the second never reached the file.")

(defparameter +conditional-result+
  (format nil "(in-package #:cl-user)~%~%(defun %reset-boundary-random-state ()~%  (setf *random-state* (make-random-state t)))~%~%#+sbcl (pushnew '%reset-boundary-random-state sb-ext:*save-hooks*)~%~%#-sbcl (uiop:register-image-dump-hook '%reset-boundary-random-state)~%~%#-sbcl (defun impl () :portable)~%~%#+sbcl (defun impl () :sbcl)~%"))

(defun demo-reader-conditionals ()
  (banner "Demo 11: #+sbcl / #-sbcl top-level forms through the edit tools (lisp-source-protocol#9)")
  (let* ((root (make-project "conditionals" `(("hooks.lisp" ,+conditional-file+))))
         (tools (lt:make-lisp-tools :root root))
         (ok t))
    (flet ((check (label value)
             (show label (and value t))
             (setf ok (and value ok))))
      (unwind-protect
           (progn
             (show "this image reads with :sbcl in *features*" (and (member :sbcl *features*) t))
             (show "hooks.lisp" +conditional-file+)
             (stage "the outline and the locators see both branches")
             (let ((outline (tool tools "source-outline" "path" "hooks.lisp")))
               (check "outline lists #+sbcl defun impl" (%has "#+sbcl defun impl" outline))
               (check "outline lists #-sbcl defun impl" (%has "#-sbcl defun impl" outline)))
             (check "path:3 is the #-sbcl form"
                    (%has "#-sbcl (defun impl () :other)"
                          (tool tools "source-read" "path" "hooks.lisp" "locator" "path:3")))
             (let ((r (tool tools "source-read" "path" "hooks.lisp" "locator" "defun impl")))
               (check "defun impl names two definitions, the #-sbcl one included"
                      (and (typep r 'condition) (%has "under #-sbcl" (princ-to-string r)))))
             (stage "edit-insert writes both forms of the http-protocol#58 save hook")
             (let ((dry (tool tools "edit-insert" "path" "hooks.lisp"
                              "locator" "defun %reset-boundary-random-state"
                              "code" +save-hook-branches+ "dry_run" t)))
               (show "the whole dry-run diff" (subseq dry (or (search "---" dry) 0)))
               (check "the dry-run diff adds the #+sbcl form" (%has "+#+sbcl (pushnew" dry))
               (check "the dry-run diff adds the #-sbcl form"
                      (%has "+#-sbcl (uiop:register-image-dump-hook" dry)))
             (tool tools "edit-insert" "path" "hooks.lisp"
                   "locator" "defun %reset-boundary-random-state" "code" +save-hook-branches+)
             (stage "edit-replace inside the #-sbcl branch, transpose of whole branches")
             (tool tools "edit-replace" "path" "hooks.lisp" "locator" "path:5.1.3" "code" ":portable")
             (tool tools "edit-structure" "op" "transpose" "path" "hooks.lisp" "locator" "path:4")
             (let ((text (read-file root "hooks.lisp")))
               (show "hooks.lisp now" text)
               (check "every branch is in the file, as written" (equal text +conditional-result+)))
             (check "the file reads clean with 6 top-level forms"
                    (%has "clean (6 top-level forms"
                          (tool tools "source-check" "path" "hooks.lisp" "rules" "none")))
             (tool tools "source-outline" "path" "hooks.lisp")
             ok)
        (lt:close-lisp-tools tools)))))
