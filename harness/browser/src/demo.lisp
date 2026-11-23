(in-package #:harness-demos-browser)

;;;; browser-protocol 0.2.0 against a real headless Chrome: the Playwright /
;;;; Selenium jobs from the python-to-cl page (launch, goto that waits for the
;;;; page, evaluate, screenshot, PDF), and the three fixes behind it:
;;;; browser-protocol#1 (stack JSON and base64), #2 (launch, replies by id,
;;;; navigate waits) and #3 (*browser-backend* for the NIL-backend methods).
;;;; Every result with a known answer is checked: RUN-DEMOS returns NIL when one
;;;; differs or a section stops on an error.

(defvar *checks* 0)
(defvar *mismatches* 0)
(defvar *failed-sections* '())

(defun shown (text)
  "TEXT with the home and temporary directories replaced by ~ and <tmp>."
  (let ((text (princ-to-string text)))
    (flet ((swap (from to)
             (let ((from (string-right-trim "/" from)))
               (loop for at = (search from text)
                     while (and at (plusp (length from)))
                     do (setf text (concatenate 'string (subseq text 0 at) to
                                                (subseq text (+ at (length from)))))))))
      (swap (namestring (uiop:temporary-directory)) "<tmp>")
      (swap (namestring (user-homedir-pathname)) "~")
      text)))

(defun banner (title)
  (format t "~&~%~72,,,'=a~%== ~a~%~72,,,'=a~%" "" title ""))

(defun para (&rest lines)
  (format t "~&~{~a~%~}~%" lines))

(defun show (label value)
  (format t "~&  ~38a ~a~%" label (shown value)))

(defun check (label got expected &key (test #'equal))
  (incf *checks*)
  (let ((ok (funcall test got expected)))
    (unless ok (incf *mismatches*))
    (format t "~&  ~:[MISMATCH~;ok      ~] ~a: ~a~:[ (want ~a)~;~*~]~%"
            ok label (shown (prin1-to-string got)) ok (prin1-to-string expected))
    ok))

(defmacro section (title &body body)
  `(progn
     (banner ,title)
     (handler-case (progn ,@body)
       (error (c)
         (push ,title *failed-sections*)
         (format t "~&  SECTION FAILED: ~a: ~a~%" (type-of c) (shown c))))))

(defun seconds-since (start)
  (/ (round (* 10 (/ (- (get-internal-real-time) start) internal-time-units-per-second))) 10.0))

(defparameter *chrome-navigate-reply*
  "{\"id\":1,\"result\":{\"frameId\":\"13AAB0BE9ACCA194BE5D8B66BD278795\",\"loaderId\":\"B5DEC2C3688A1476E494B6B15132FBA5\",\"isDownload\":false}}"
  "Chrome's Page.navigate reply, verbatim (browser-protocol#1).")

(defun png-size (octets)
  "Width and height from a PNG's IHDR chunk."
  (flet ((u32 (at) (reduce (lambda (acc b) (+ (* acc 256) b)) (subseq octets at (+ at 4)))))
    (list (u32 16) (u32 20))))

(defun demo-chrome-json ()
  (section "1. Chrome's JSON and base64 go through the stack (#1)"
    (para "0.1.0 parsed CDP JSON with a private parser whenever no json-protocol backend"
          "was bound, and that parser could not read `false`: Chrome's Page.navigate reply"
          "(\"isDownload\":false) failed with \"bad JSON false\". 0.2.0 depends on"
          "json-protocol + json-backend-jzon and encoding-protocol; the private parser and"
          "base64 are gone.")
    (let* ((msg (b:decode-cdp-message *chrome-navigate-reply*))
           (result (b:cdp-message-result msg)))
      (show "decode-cdp-message →" (type-of msg))
      (check "frameId" (gethash "frameId" result) "13AAB0BE9ACCA194BE5D8B66BD278795")
      (multiple-value-bind (value present) (gethash "isDownload" result)
        (check "isDownload (false → NIL, present)" (list value present) '(nil t))))
    (check "null → :NULL"
           (gethash "x" (b:cdp-message-result (b:decode-cdp-message "{\"id\":2,\"result\":{\"x\":null}}")))
           :null)
    (check "symbols gone: %PARSE-JSON %BASE64-DECODE"
           (loop for name in '("%PARSE-JSON" "%BASE64-ENCODE" "%BASE64-DECODE")
                 for s = (find-symbol name :browser-protocol)
                 count (and s (fboundp s)))
           0)
    (handler-case (b:decode-cdp-message "{\"id\":")
      (b:browser-error (c)
        (show "malformed JSON →" (type-of c))
        (check "its cause is a json-protocol condition"
               (typep (b:browser-error-cause c) 'json-protocol:json-error) t)))))

(defun demo-launch (browser)
  (section "2. Launch: chromium.launch() + new_page() (#2)"
    (para "LAUNCH-BROWSER starts the browser headless with --remote-debugging-port=0 and a"
          "temporary --user-data-dir in its own process group, reads the port from"
          "DevToolsActivePort, opens the browser websocket (ws-protocol) and attaches to the"
          "page with a flattened session: one socket, page commands carry the sessionId.")
    (show "find-browser-executable" (b:find-browser-executable))
    (show "process alive-p" (stack-process:alive-p (b:chrome-browser-process browser)))
    (show "profile" (b:chrome-browser-profile browser))
    (show "browser websocket"
          (let ((url (b:cdp-ws-url browser)))
            (subseq url 0 (or (search "/devtools/" url) (length url)))))
    (show "session id (flatten)" (format nil "~a… (~d chars)"
                                          (subseq (b:cdp-session-id browser) 0 8)
                                          (length (b:cdp-session-id browser))))
    (show "user agent" (b:eval-js browser "navigator.userAgent"))
    (check "connected" (b::cdp-connected-p browser) t)))

(defun demo-replies-by-id (browser)
  (section "3. Replies are matched by id while events arrive (#2)"
    (para "CONNECT-BROWSER sent Target.setDiscoverTargets; Chrome answers with a"
          "Target.targetCreated event per target before the reply. 0.1.0 took the first event"
          "as the reply and every later EVAL-JS read the next event (NIL, NIL, NIL). 0.2.0"
          "reads until the reply with its id and keeps the events in a bounded buffer.")
    (let ((created (loop for event = (handler-case
                                         (b:wait-for-cdp-event browser "Target.targetCreated" :timeout 0)
                                       (b:browser-timeout () nil))
                         while event
                         collect (gethash "type" (gethash "targetInfo" event)))))
      (show "Target.targetCreated events buffered" (format nil "~d: ~{~a~^, ~}" (length created) created))
      (check "at least the page" (and (member "page" created :test #'equal) t) t))
    (check "eval-js 1+1, 2+2, 3+3"
           (list (b:eval-js browser "1+1") (b:eval-js browser "2+2") (b:eval-js browser "3+3"))
           '(2 4 6))))

(defun demo-navigate (browser)
  (section "4. NAVIGATE waits for the page (#2)"
    (para "Playwright's page.goto waits for `load` by default. NAVIGATE now takes"
          ":wait-until (:load, :domcontentloaded, :commit) and :timeout; it sends Page.enable"
          "once and waits for the load event of the new loader.")
    (b:navigate browser "data:text/html,<title>fast</title><h1>hello</h1>")
    (check "default (:load): readyState" (b:eval-js browser "document.readyState") "complete")
    (check "title" (b:eval-js browser "document.title") "fast")
    (let ((slow "data:text/html,<title>slow</title><img src='http://10.255.255.1/never.png'>")
          (start (get-internal-real-time)))
      (para ""
            "A page whose image never arrives (10.255.255.1 does not answer): DOMContentLoaded"
            "fires at once, load does not.")
      (b:navigate browser slow :wait-until :domcontentloaded)
      (show ":domcontentloaded returned after" (format nil "~,1fs" (seconds-since start)))
      (check ":domcontentloaded: readyState" (b:eval-js browser "document.readyState") "interactive")
      (setf start (get-internal-real-time))
      (handler-case (progn (b:navigate browser slow :timeout 2)
                           (check ":load with :timeout 2 signals" :returned 'b:browser-timeout))
        (b:browser-timeout (c)
          (show ":load, :timeout 2 →" (format nil "~a after ~,1fs" (type-of c) (seconds-since start)))
          (show "message" c)
          (check "browser-error-url" (b:browser-error-url c) slow))))
    (b:navigate browser "about:blank")
    (b:navigate browser "about:blank#part")
    (check "#fragment (same document, no load event)" (b:eval-js browser "location.hash") "#part")
    (handler-case (progn (b:navigate browser "http://no-such-host.invalid/")
                         (check "unresolvable host signals" :returned 'b:browser-error))
      (b:browser-error (c)
        (show "unresolvable host →" (type-of c))
        (check "message names Chrome's errorText"
               (and (search "ERR_NAME_NOT_RESOLVED" (princ-to-string c)) t) t)))))

(defun demo-events (browser)
  (section "5. Events: ON-CDP-EVENT and WAIT-FOR-CDP-EVENT"
           (para "Handlers run on the reading thread for every event read while a command or a"
                 "wait reads. Events are also kept in a bounded buffer, and WAIT-FOR-CDP-EVENT"
                 "takes the oldest matching one from it first, so an event that came in during the"
                 "previous command is not missed. :test narrows the wait.")
           (let ((logged '()))
             (flet ((message (params) (gethash "value" (aref (gethash "args" params) 0))))
               (b:cdp-call browser "Runtime.enable")
               (b:on-cdp-event browser "Runtime.consoleAPICalled"
                               (lambda (params) (push (message params) logged)))
               (check "eval-js \"console.log('hello from the page'), 42\""
                      (b:eval-js browser "console.log('hello from the page'), 42") 42)
               (check "the handler saw the console event" (reverse logged) '("hello from the page"))
               (b:eval-js browser "setTimeout(() => console.log('later'), 300), 0")
               (check "wait-for-cdp-event: the buffered one first"
                      (message (b:wait-for-cdp-event browser "Runtime.consoleAPICalled" :timeout 5))
                      "hello from the page")
               (let* ((start (get-internal-real-time))
                      (params (b:wait-for-cdp-event browser "Runtime.consoleAPICalled"
                                                    :timeout 5
                                                    :test (lambda (p) (equal "later" (message p))))))
                 (show "the :test wait returned after" (format nil "~,1fs" (seconds-since start)))
                 (check "its message" (message params) "later"))
               (check "the handler saw both" (reverse logged) '("hello from the page" "later"))
               (b:off-cdp-event browser "Runtime.consoleAPICalled")))))

(defun demo-octets (browser)
  (section "6. Screenshot and PDF: base64 through encoding-protocol (#1)"
    (b:navigate browser "data:text/html,<title>shot</title><body style='background:%23336'><h1 style='color:white'>browser-protocol 0.2.0</h1>")
    (let ((png (b:screenshot browser)))
      (show "screenshot" (format nil "~:d bytes, ~{~d×~d~} px" (length png) (png-size png)))
      (check "PNG signature" (coerce (subseq png 0 4) 'list) '(137 80 78 71)))
    (let ((pdf (b:print-pdf browser)))
      (show "print-pdf" (format nil "~:d bytes" (length pdf)))
      (check "PDF header" (map 'string #'code-char (subseq pdf 0 5)) "%PDF-"))
    (show "dom-snapshot" (let ((dom (b:dom-snapshot browser)))
                           (subseq dom (search "<h1" dom) (+ (search "</h1>" dom) 5))))))

(defun demo-connect (browser)
  (section "7. CONNECT-CDP: a second client on a running browser"
    (para "connect_over_cdp: CONNECT-CDP attaches its own session; its CLOSE-BROWSER detaches"
          "and leaves the browser running.")
    (let ((other (b:connect-cdp (b:cdp-ws-url browser))))
      (check "other client: eval-js 6*7" (b:eval-js other "6*7") 42)
      (b:close-browser other)
      (check "browser still alive after the other client closed"
             (stack-process:alive-p (b:chrome-browser-process browser)) t)
      (check "launched client still works" (b:eval-js browser "1+1") 2))))

(defun demo-close (browser)
  (section "8. CLOSE-BROWSER"
    (let ((process (b:chrome-browser-process browser))
          (profile (b:chrome-browser-profile browser)))
      (b:close-browser browser)
      (check "process alive-p" (stack-process:alive-p process) nil)
      (check "temporary profile exists" (and (probe-file profile) t) nil))))

(defun demo-with-browser ()
  (section "9. WITH-BROWSER and *BROWSER-BACKEND* (#3)"
    (para "WITH-BROWSER binds the backend and *BROWSER-BACKEND*; the NIL-backend methods now"
          "use *BROWSER-BACKEND* (0.1.0 ignored it).")
    (let (process)
      (b:with-browser (w)
        (setf process (b:chrome-browser-process w))
        (b:navigate nil "data:text/html,<title>with-browser</title>")
        (check "(eval-js nil \"document.title\")" (b:eval-js nil "document.title") "with-browser"))
      (check "closed on the way out" (stack-process:alive-p process) nil))))

(defun run-demos ()
  "Run every section. → T when every check matched and no section failed."
  (setf *checks* 0 *mismatches* 0 *failed-sections* '())
  (demo-chrome-json)
  (cond
    ((null (b:find-browser-executable))
     (format t "~&~%No Chrome / Chromium / Edge found (set CHROME_PATH): sections 2-9 skipped.~%")
     (push "no browser" *failed-sections*))
    (t
     (let ((browser (b:launch-browser)))
       (unwind-protect
            (progn
              (demo-launch browser)
              (demo-replies-by-id browser)
              (demo-navigate browser)
              (demo-events browser)
              (demo-octets browser)
              (demo-connect browser)
              (demo-close browser))
         (b:close-browser browser)))
     (demo-with-browser)))
  (format t "~&~%~d checks, ~d mismatched~@[; failed sections: ~{~a~^, ~}~]~%"
          *checks* *mismatches* (reverse *failed-sections*))
  (and (zerop *mismatches*) (null *failed-sections*)))
