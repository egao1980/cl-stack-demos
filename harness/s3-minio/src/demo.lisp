;;;; A tour of object-store-backend-s3 0.2.0 against a live MinIO container,
;;;; with cl-stack-config 0.3.1's parse-ini underneath (cl-stack-config#10):
;;;; the credential chain from the environment and from an ~/.aws-style
;;;; credentials file whose secret holds # and ;, put / get / head / list, a
;;;; presigned GET, paged listings, a multipart upload, and delete. Every
;;;; result with a known answer is checked: RUN-DEMOS returns NIL when one
;;;; differs or a section stops on an error.
;;;;
;;;; scripts/run.sh starts the container with credentials it makes up for the
;;;; run, passes them in AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY and removes
;;;; the container afterwards. Neither the key nor the secret is printed.

(in-package #:harness-demos-s3-minio)

;;; Output and checks ---------------------------------------------------------

(defvar *checks* 0 "Results compared with their expected value.")
(defvar *mismatches* 0 "Results that differed from their expected value.")
(defvar *failed-sections* '() "Titles of sections that stopped on an error.")

(defun out (control &rest args)
  (apply #'format t control args))

(defun say (&rest lines)
  "Print each of LINES on its own line, indented by two spaces (\"\" is a blank line)."
  (dolist (line lines)
    (if (string= line "")
        (terpri)
        (out "  ~a~%" line))))

(defun banner (title)
  (format t "~&~%~78,,,'=a~%== ~a~%~78,,,'=a~%" "" title ""))

(defun check (actual expected label &key (test #'equal))
  "Count one self-check; print it only when it fails. Returns ACTUAL."
  (incf *checks*)
  (unless (funcall test actual expected)
    (incf *mismatches*)
    (let ((*print-pretty* nil))
      (out "  !! MISMATCH ~a: got ~s, expected ~s~%" label actual expected)))
  actual)

(defmacro section (title &body body)
  "Run BODY under a banner; an error ends this section only."
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (c)
         (push ,title *failed-sections*)
         (out "~&!! section stopped on an error: ~a~%   (~s)~%" c (type-of c))))
     (finish-output)))

;;; The MinIO container and its credentials ----------------------------------

(defvar *endpoint* nil "http://127.0.0.1:PORT of the MinIO container.")
(defvar *bucket* nil "The bucket scripts/run.sh created.")
(defvar *access-key* nil "The throwaway access key scripts/run.sh made up.")
(defvar *secret-key* nil "The throwaway secret scripts/run.sh made up; it holds # and ;.")
(defvar *scratch* nil "A temporary directory for the credentials files.")

(defun setting (name)
  (let ((value (uiop:getenv name)))
    (if (and value (plusp (length value)))
        value
        (error "~a is not set: run the demo with scripts/run.sh" name))))

(defun set-env (name value)
  "Set the environment variable NAME to VALUE; NIL sets it empty, which the
   S3 credential chain reads as unset (as botocore does)."
  (setf (uiop:getenv name) (or value "")))

(defun secret-shape (secret)
  "Where # and ; are in SECRET, never SECRET itself."
  (format nil "~d characters, # at index ~d, ; at index ~d"
          (length secret) (position #\# secret) (position #\; secret)))

(defun endpoint-shown ()
  "*ENDPOINT* with its random port written as <port>."
  (let ((colon (position #\: *endpoint* :from-end t)))
    (format nil "~a<port>" (subseq *endpoint* 0 (1+ colon)))))

(defun store (&rest keys)
  "An S3-BACKEND for the MinIO container; KEYS go to MAKE-S3-BACKEND."
  (apply #'s3:make-s3-backend :endpoint *endpoint* :bucket *bucket* keys))

(defun provider-of (backend)
  "The first of *S3-CREDENTIAL-PROVIDERS* that yields keys for BACKEND."
  (loop for provider in s3:*s3-credential-providers*
        when (funcall provider backend)
          return provider))

(defun text (octets)
  (encoding-protocol:decode octets :encoding :utf-8))

(defun s3-error-code (condition)
  "The <Code> of the S3 error document in CONDITION's message, else its text."
  (let* ((message (princ-to-string condition))
         (start (search "<Code>" message))
         (end (and start (search "</Code>" message :start2 start))))
    (if end
        (subseq message (+ start 6) end)
        message)))

(defun s3-status (condition)
  "The HTTP status in an object-store-error message (\"S3 HTTP 403 ...\")."
  (let* ((message (princ-to-string condition))
         (at (search "HTTP " message)))
    (and at (parse-integer message :start (+ at 5) :junk-allowed t))))

;;; 1. The environment -------------------------------------------------------

(defun demo-environment ()
  (say (format nil "Endpoint ~a, bucket ~s. AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY hold"
               (endpoint-shown) *bucket*)
       "the throwaway credentials scripts/run.sh made up for this container. The"
       (format nil "secret: ~a. AWS_SHARED_CREDENTIALS_FILE and" (secret-shape *secret-key*))
       "AWS_CONFIG_FILE name files that do not exist yet, so ~/.aws is never read."
       "")
  (let ((s3 (store)))
    (say "(make-s3-backend :endpoint ... :bucket ...), no keys passed:")
    (let ((provider (provider-of s3)))
      (say (format nil "  first provider with keys: ~(~a~)" provider))
      (check provider 's3:environment-credentials "provider"))
    (let ((stat (os:put-object s3 "env/hello.txt" "hello from the environment"
                               :content-type "text/plain")))
      (say (format nil "put-object env/hello.txt (26 octets) -> etag ~a" (os:object-stat-etag stat)))
      (check (length (os:object-stat-etag stat)) 34 "etag: 32 hex digits in quotes"))
    (let ((body (text (os:get-object s3 "env/hello.txt"))))
      (say (format nil "get-object env/hello.txt -> ~s" body))
      (check body "hello from the environment" "get-object"))
    (let* ((stat (os:head-object s3 "env/hello.txt"))
           (modified (os:object-stat-last-modified stat)))
      (say (format nil "head-object -> size ~d, content-type ~a, Last-Modified ~:[missing~;an HTTP date (~d characters)~]"
                   (os:object-stat-size stat) (os:object-stat-content-type stat)
                   modified (length modified))
           "  (object-store-backend-s3#10, 0.2.1: 0.2.0 gave size 0 and no Last-Modified)")
      (check (os:object-stat-size stat) 26 "head-object size")
      (check (os:object-stat-content-type stat) "text/plain" "head-object content-type")
      (check (and modified (search " GMT" modified) t) t "head-object Last-Modified"))
    (let ((keys (mapcar #'os:object-stat-key
                        (os:object-listing-objects (os:list-objects s3 :prefix "env/")))))
      (say (format nil "list-objects :prefix \"env/\" -> ~s" keys))
      (check keys '("env/hello.txt") "list-objects"))
    (let* ((url (os:presign-url s3 "env/hello.txt" :expires 300))
           (response (http:request :get url
                                            :backend (cl-stack-http:ensure-http-backend)
                                            :raise-for-status nil))
           (status (http-protocol:response-status response))
           (body (http-protocol:response-body response)))
      (say (format nil "presign-url env/hello.txt :expires 300, fetched with no credentials (#7):")
           (format nil "  ~a?X-Amz-Algorithm=AWS4-HMAC-SHA256&...&X-Amz-Signature=<64 hex>"
                   (subseq url (length *endpoint*) (position #\? url)))
           (format nil "  -> HTTP ~d ~s" status (if (stringp body) body (text body))))
      (check status 200 "presigned GET status")
      (check (if (stringp body) body (text body)) "hello from the environment" "presigned GET body"))))

;;; 2. A credentials file with # and ; in the secret ------------------------

(defparameter +credentials-template+
  "# throwaway credentials made up for this MinIO container
[Default]
aws_access_key_id = not-this-one
aws_secret_access_key = not-this-one-either

[default]
AWS_ACCESS_KEY_ID = ~a
aws_secret_access_key = ~a
"
  "The credentials file: [default] holds the container's keys under an
   upper-case and a lower-case name; [Default] is another section.")

(defparameter +configparser-script+
  "import configparser, hashlib, sys
cp = configparser.RawConfigParser()
cp.read(sys.argv[1], encoding='utf-8')
for section in cp.sections():
    for option in cp.options(section):
        value = cp.get(section, option)
        print(section, option, len(value), hashlib.sha256(value.encode()).hexdigest(), sep='\\t')
"
  "botocore's reading of argv[1]: RawConfigParser() with its defaults. Each
   value is printed as its length and SHA-256, never as itself.")

(defun uv-available-p ()
  (handler-case (zerop (proc:run '("uv" "--version") :output nil :error-output nil :timeout 60))
    (error () nil)))

(defun parse-ini-rows (table)
  "PARSE-INI's TABLE as the lines +CONFIGPARSER-SCRIPT+ prints, sorted."
  (let ((rows '()))
    (maphash (lambda (section options)
               (when (hash-table-p options)
                 (maphash (lambda (option value)
                            (push (format nil "~a~c~a~c~d~c~a" section #\Tab option #\Tab
                                          (length value) #\Tab (s3:octets-to-hex (s3:sha256 value)))
                                  rows))
                          options)))
             table)
    (sort rows #'string<)))

(defun demo-credentials-file ()
  (let ((path (merge-pathnames "credentials" *scratch*)))
    (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-8)
      (format o +credentials-template+ *access-key* *secret-key*))
    (say "The credentials file, with the container's key and secret written as <KEY> and <SECRET>:"
         "")
    (dolist (line (uiop:split-string (format nil +credentials-template+ "<KEY>" "<SECRET>")
                                     :separator '(#\Newline)))
      (out "    | ~a~%" line))
    (let* ((ini (cfg:parse-ini (uiop:read-file-string path) path))
           (default (gethash "default" ini))
           (sections (sort (loop for k being the hash-keys of ini using (hash-value v)
                                 when (hash-table-p v) collect k)
                           #'string<)))
      (say ""
           "cl-stack-config 0.3.1 parse-ini, as botocore reads the file:"
           (format nil "  sections ~s (names keep their case)" sections)
           (format nil "  [default] options ~s (names lowercased)"
                   (sort (loop for k being the hash-keys of default collect k) #'string<))
           (format nil "  aws_secret_access_key: ~a, equal to the container's secret: ~:[no~;yes~]"
                   (secret-shape (gethash "aws_secret_access_key" default))
                   (equal *secret-key* (gethash "aws_secret_access_key" default))))
      (check sections '("Default" "default") "sections")
      (check (gethash "aws_access_key_id" default) *access-key* "aws_access_key_id from AWS_ACCESS_KEY_ID")
      (check (gethash "aws_secret_access_key" default) *secret-key* "the whole secret")
      (say "parse-ini 0.3.0 cut every line at its first # or ; and kept name case: it read"
           (format nil "  aws_secret_access_key as the ~d characters before the #, and found no"
                   (position #\# *secret-key*))
           "  aws_access_key_id (the file says AWS_ACCESS_KEY_ID).")
      (if (not (uv-available-p))
          (say "" "uv is not on PATH, so the cross-read with Python's configparser is skipped.")
          (multiple-value-bind (code stdout stderr)
              (proc:run (list "uv" "run" "--isolated" "--no-project" "python" "-I" "-c"
                              +configparser-script+ (uiop:native-namestring path))
                        :output :string :error-output :string :timeout 600
                        :env '(("PYTHONIOENCODING" . "utf-8")))
            (let ((python (sort (remove "" (uiop:split-string (string-right-trim '(#\Newline) stdout)
                                                              :separator '(#\Newline))
                                        :test #'string=)
                                #'string<))
                  (lisp (parse-ini-rows ini)))
              (check code 0 "uv run exit code")
              (when (plusp (length stderr))
                (say (format nil "  python stderr: ~a" stderr)))
              (say ""
                   "Python's configparser.RawConfigParser() on the same file (uv run --isolated"
                   "--no-project python -I), each value as its length and SHA-256:")
              (dolist (row python)
                (destructuring-bind (section option len hash)
                    (uiop:split-string row :separator '(#\Tab))
                  (say (format nil "  [~a] ~a: ~a characters, sha256 ~a..." section option len
                               (subseq hash 0 12)))))
              (say (format nil "  -> ~:[differs from~;the same as~] parse-ini, value for value"
                           (equal python lisp)))
              (check python lisp "configparser = parse-ini")))))
    (say ""
         "Signed with the 0.3.0 reading of the secret (the 4 characters before the #):")
    (handler-case
        (progn (os:put-object (store :access-key *access-key*
                                     :secret-key (subseq *secret-key* 0 (position #\# *secret-key*)))
                              "file/cut.txt" "x")
               (say "  !! MinIO took it")
               (check :accepted :refused "cut secret"))
      (os:object-store-error (e)
        (say (format nil "  put-object -> ~(~a~): HTTP ~a ~a" (type-of e) (s3-status e) (s3-error-code e)))
        (check (s3-error-code e) "SignatureDoesNotMatch" "cut secret refused")))
    (say ""
         "Now the chain alone: AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY cleared,"
         "AWS_SHARED_CREDENTIALS_FILE set to this file.")
    (set-env "AWS_ACCESS_KEY_ID" nil)
    (set-env "AWS_SECRET_ACCESS_KEY" nil)
    (set-env "AWS_SHARED_CREDENTIALS_FILE" (uiop:native-namestring path))
    (let ((s3 (store)))
      (let ((provider (provider-of s3)))
        (say (format nil "  first provider with keys: ~(~a~)" provider))
        (check provider 's3:shared-credentials-file-credentials "provider"))
      (os:put-object s3 "file/from-credentials-file.txt" "signed with the secret from the file")
      (let ((body (text (os:get-object s3 "file/from-credentials-file.txt"))))
        (say (format nil "  put-object then get-object file/from-credentials-file.txt -> ~s" body))
        (check body "signed with the secret from the file" "get-object")))))

;;; 3. Multipart -----------------------------------------------------------

(defun pattern-octets (n seed)
  "N octets of a fixed pattern."
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n v)
      (setf (aref v i) (mod (+ seed (* i 31)) 251)))))

(defun demo-multipart ()
  (let* ((s3 (store))
         (part-1 (pattern-octets (* 5 1024 1024) 1))
         (part-2 (pattern-octets (+ (* 1024 1024) 123) 2))
         (whole (concatenate '(vector (unsigned-byte 8)) part-1 part-2))
         (key "multipart/blob.bin"))
    (say "Credentials from the file of section 2. Part 1 is 5 MiB, the S3 minimum for every"
         "part but the last; part 2 is 1 MiB + 123 octets.")
    (let* ((id (os:create-multipart-upload s3 key :content-type "application/octet-stream"))
           (etag-1 (os:upload-part s3 key id 1 part-1))
           (etag-2 (os:upload-part s3 key id 2 part-2))
           (stat (os:complete-multipart-upload s3 key id (list (cons 1 etag-1) (cons 2 etag-2))))
           (etag (os:object-stat-etag stat)))
      (say (format nil "create-multipart-upload ~a :content-type application/octet-stream -> an upload id (~d characters)~%    (the signed Content-Type of this bodyless POST reaches MinIO since http-backend-dexador 0.2.5, #36)"
                   key (length id))
           (format nil "upload-part 1 -> etag ~a" etag-1)
           (format nil "upload-part 2 -> etag ~a" etag-2)
           (format nil "complete-multipart-upload -> etag ~a" etag))
      (check (and etag (search "-2" etag) t) t "multipart etag ends in -2")
      (let ((head (os:head-object s3 key)))
        (say (format nil "head-object -> size ~d" (os:object-stat-size head)))
        (check (os:object-stat-size head) (length whole) "size"))
      (let ((body (os:get-object s3 key)))
        (say (format nil "get-object -> ~d octets, the two parts in order: ~:[no~;yes~]"
                     (length body) (equalp body whole)))
        (check (equalp body whole) t "get-object = part 1 + part 2")))
    (say ""
         "Two 1 KiB parts: MinIO refuses to complete, the upload is aborted, and a part"
         "sent after the abort finds no upload:")
    (let* ((key "multipart/too-small.bin")
           (id (os:create-multipart-upload s3 key))
           (parts (list (cons 1 (os:upload-part s3 key id 1 (pattern-octets 1024 3)))
                        (cons 2 (os:upload-part s3 key id 2 (pattern-octets 1024 4))))))
      (handler-case
          (progn (os:complete-multipart-upload s3 key id parts)
                 (check :completed :refused "too-small parts"))
        (os:object-store-error (e)
          (say (format nil "  complete-multipart-upload -> ~(~a~): HTTP ~a ~a"
                       (type-of e) (s3-status e) (s3-error-code e)))
          (check (s3-error-code e) "EntityTooSmall" "too-small parts refused")))
      (os:abort-multipart-upload s3 key id)
      (say "  abort-multipart-upload -> done")
      (handler-case
          (progn (os:upload-part s3 key id 3 (pattern-octets 16 5))
                 (check :uploaded :refused "part after abort"))
        (os:object-store-error (e)
          (say (format nil "  upload-part after the abort -> ~(~a~)~@[: HTTP ~a~] ~a" (type-of e)
                       (s3-status e) (s3-error-code e)))
          (check (and (typep e 'os:object-store-error) t) t "part after abort refused"))))
    (let ((keys (mapcar #'os:object-stat-key
                        (os:object-listing-objects (os:list-objects s3 :prefix "multipart/")))))
      (say (format nil "list-objects :prefix \"multipart/\" -> ~s" keys))
      (check keys '("multipart/blob.bin") "only the completed upload is listed"))))

;;; 4. Paged listings --------------------------------------------------------

(defun demo-paging ()
  (let ((s3 (store)))
    (dotimes (i 5)
      (os:put-object s3 (format nil "page/~d.txt" i) (format nil "object ~d" i)))
    (say "Five objects page/0.txt .. page/4.txt, listed :max-keys 2 at a time:")
    (let ((pages '())
          (token nil))
      (loop
        (let* ((listing (os:list-objects s3 :prefix "page/" :max-keys 2 :continuation-token token))
               (keys (mapcar #'os:object-stat-key (os:object-listing-objects listing))))
          (push keys pages)
          (say (format nil "  page ~d: ~s, truncated ~:[no~;yes~]" (length pages) keys
                       (os:object-listing-truncated-p listing)))
          (setf token (os:object-listing-continuation-token listing))
          (unless (and (os:object-listing-truncated-p listing) token (< (length pages) 10))
            (return))))
      (check (reverse pages) '(("page/0.txt" "page/1.txt") ("page/2.txt" "page/3.txt") ("page/4.txt"))
             "pages"))))

;;; 5. Delete ------------------------------------------------------------------

(defun demo-delete ()
  (let ((s3 (store)))
    (os:delete-object s3 "env/hello.txt")
    (say "delete-object env/hello.txt, then get-object env/hello.txt:")
    (handler-case
        (progn (os:get-object s3 "env/hello.txt")
               (check :found :not-found "get after delete"))
      (os:object-not-found (e)
        (say (format nil "  -> ~(~a~): ~a" (type-of e) e))
        (check (os:object-store-error-key e) "env/hello.txt" "object-not-found key")))))

;;; Run ---------------------------------------------------------------------

(defun run-demos ()
  "Print the tour. Returns T when every check matched and no section failed."
  (let ((*checks* 0) (*mismatches* 0) (*failed-sections* '())
        (*print-right-margin* 110)
        (*endpoint* (setting "S3_DEMO_ENDPOINT"))
        (*bucket* (setting "S3_DEMO_BUCKET"))
        (*access-key* (setting "AWS_ACCESS_KEY_ID"))
        (*secret-key* (setting "AWS_SECRET_ACCESS_KEY"))
        (*scratch* (uiop:ensure-directory-pathname
                    (merge-pathnames (format nil "harness-s3-minio-~36r" (random (expt 36 6)
                                                                                 (make-random-state t)))
                                     (uiop:temporary-directory)))))
    (ensure-directories-exist *scratch*)
    ;; Keep ~/.aws and any profile of the caller out of the chain.
    (set-env "AWS_SHARED_CREDENTIALS_FILE" (uiop:native-namestring (merge-pathnames "none" *scratch*)))
    (set-env "AWS_CONFIG_FILE" (uiop:native-namestring (merge-pathnames "config" *scratch*)))
    (dolist (name '("AWS_PROFILE" "AWS_DEFAULT_PROFILE" "AWS_SESSION_TOKEN" "AWS_SECURITY_TOKEN"))
      (set-env name nil))
    (set-env "AWS_REGION" "us-east-1")
    (format t "~&object-store-backend-s3 ~a, cl-stack-config ~a, cl-stack-http ~a (~a ~a, ~a)~%"
            (asdf:component-version (asdf:find-system "object-store-backend-s3"))
            (asdf:component-version (asdf:find-system "cl-stack-config"))
            (asdf:component-version (asdf:find-system "cl-stack-http"))
            (lisp-implementation-type) (lisp-implementation-version) (software-type))
    (format t "MinIO: ~a; HTTP backend for blocking calls: ~(~a~)~%"
            (or (uiop:getenv "S3_DEMO_MINIO") "unknown")
            (type-of (cl-stack-http:ensure-http-backend)))
    (unwind-protect
         (progn
           (section "1. Credentials from the environment: put, get, head, list, presigned GET"
                    (demo-environment))
           (section "2. Credentials from an ~/.aws-style file with # and ; in the secret (cl-stack-config#10)"
                    (demo-credentials-file))
           (section "3. Multipart upload: a 5 MiB part and a tail; EntityTooSmall; abort"
                    (demo-multipart))
           (section "4. Paged listings" (demo-paging))
           (section "5. Delete" (demo-delete)))
      (uiop:delete-directory-tree *scratch* :validate t :if-does-not-exist :ignore))
    (banner "Summary")
    (format t "~d results checked against their expected values: ~d mismatched.~%~
               ~:[Every section ran to the end.~;Sections that stopped on an error: ~:*~{~a~^; ~}.~]~%"
            *checks* *mismatches* (reverse *failed-sections*))
    (finish-output)
    (and (zerop *mismatches*) (null *failed-sections*))))
